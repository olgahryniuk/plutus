-- editorconfig-checker-disable-file
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

{- |
An inlining pass.

This pass is essentially a copy of the PIR inliner, and should be KEPT IN SYNC with it.
It's hard to do this with true abstraction, so we just have to keep two copies reasonably
similar.

However, there are some differences. In the interests of making it easier to keep
things in sync, these are explicitly listed in Note [Differences from PIR inliner].
If you add another difference, please note it there! Obviously fewer differences is
better.

See Note [The problem of inlining destructors] for why this pass exists.
-}
module UntypedPlutusCore.Transform.Inline (inline, InlineHints (..)) where

import PlutusCore qualified as PLC
import PlutusCore.Annotation
import PlutusCore.Builtin qualified as PLC
import PlutusCore.MkPlc (mkIterApp)
import PlutusCore.Name
import PlutusCore.Quote
import PlutusCore.Rename (Dupable, dupable, liftDupable)
import PlutusCore.Rename.Internal (Dupable (Dupable))
import PlutusPrelude
import UntypedPlutusCore.Analysis.Usages qualified as Usages
import UntypedPlutusCore.Core qualified as UPLC
import UntypedPlutusCore.Core.Plated
import UntypedPlutusCore.Core.Type
import UntypedPlutusCore.MkUPlc
import UntypedPlutusCore.Size
import UntypedPlutusCore.Subst

import Control.Lens hiding (Strict)
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import GHC.IO (unsafePerformIO)
import PlutusCore.Default
import Unsafe.Coerce (unsafeCoerce)
import UntypedPlutusCore.Rename (Rename (rename))
import Witherable (wither)

{- Note [Differences from PIR inliner]
See the module comment for explanation for why this exists and is similar to the PIR inliner.

1. No types (obviously).
2. No strictness information (we only have lambda arguments, which are always strict).
3. Handling of multiple beta-reductions in one go, this is handled in PIR by a dedicated pass.
4. Simplistic purity analysis, in particular we don't try to be clever about builtins
(should mostly be handled in PIR).
-}

-- | Substitution range, 'SubstRng' in the paper.
newtype InlineTerm name uni fun a = Done (Dupable (Term name uni fun a))

{- | Term substitution, 'Subst' in the paper.
A map of unprocessed variable and its substitution range.
-}
newtype TermEnv name uni fun a = TermEnv
  {_unTermEnv :: PLC.UniqueMap TermUnique (InlineTerm name uni fun a)}
  deriving newtype (Semigroup, Monoid)

{- | Wrapper of term substitution so that it's similar to the PIR inliner.
See Note [Differences from PIR inliner] 1
-}
newtype Subst name uni fun a = Subst {_termEnv :: TermEnv name uni fun a}
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

makeLenses ''TermEnv
makeLenses ''Subst

newtype VarInfo name uni fun ann = VarInfo
  { _varRhs     :: InlineTerm name uni fun ann
   -- ^ its definition that has been unconditionally inlined, as an `InlineTerm`. To preserve
    -- global uniqueness, we rename before substituting in.
  }

makeLenses ''VarInfo

-- | UPLC inliner state
data S name uni fun a = S
  { _subst :: Subst name uni fun a
  , _vars  :: PLC.UniqueMap TermUnique (VarInfo name uni fun a)
  }

makeLenses ''S

instance Semigroup (S name uni fun a) where
  S a1 b1 <> S a2 b2 = S (a1 <> a2) (b1 <> b2)

instance Monoid (S name uni fun a) where
  mempty = S mempty mempty

type ExternalConstraints name uni fun m =
  ( HasUnique name TermUnique
  , Eq name
  , PLC.ToBuiltinMeaning uni fun
  , MonadQuote m
  )

type InliningConstraints name uni fun =
  ( HasUnique name TermUnique
  , Eq name
  , PLC.ToBuiltinMeaning uni fun
  )

-- See Note [Differences from PIR inliner] 2
data InlineInfo name a = InlineInfo {_usages :: Usages.Usages, _hints :: InlineHints name a}

-- Using a concrete monad makes a very large difference to the performance of this module
-- (determined from profiling)

-- | The monad the inliner runs in.
type InlineM name uni fun a = ReaderT (InlineInfo name a) (StateT (S name uni fun a) Quote)

-- | Look up the unprocessed variable in the substitution.
lookupTerm ::
  (HasUnique name TermUnique) =>
  name ->
  S name uni fun a ->
  Maybe (InlineTerm name uni fun a)
lookupTerm n s = lookupName n $ s ^. subst . termEnv . unTermEnv

-- | Insert the unprocessed variable into the substitution.
extendTerm ::
  (HasUnique name TermUnique) =>
  -- | The name of the variable.
  name ->
  -- | The substitution range.
  InlineTerm name uni fun a ->
  -- | The substitution.
  S name uni fun a ->
  S name uni fun a
extendTerm n clos s = s & subst . termEnv . unTermEnv %~ insertByName n clos

lookupVarInfo ::
  (HasUnique name TermUnique) =>
  name ->
  S name uni fun a ->
  Maybe (VarInfo name uni fun a)
lookupVarInfo n s = lookupName n $ s ^. vars

extendVarInfo ::
  (HasUnique name TermUnique) =>
  name ->
  VarInfo name uni fun a ->
  S name uni fun a ->
  S name uni fun a
extendVarInfo n info s = s & vars %~ insertByName n info

{- | Inline simple bindings. Relies on global uniqueness, and preserves it.
See Note [Inlining and global uniqueness]
-}
inline ::
  forall name uni fun m a.
  (ExternalConstraints name uni fun m) =>
  InlineHints name a ->
  Term name uni fun a ->
  m (Term name uni fun a)
inline hints t =
  let
    inlineInfo :: InlineInfo name a
    inlineInfo = InlineInfo usgs hints
    usgs :: Usages.Usages
    usgs = Usages.termUsages t
   in
    liftQuote $ flip evalStateT mempty $ flip runReaderT inlineInfo $ processTerm t

-- See Note [Differences from PIR inliner] 3

{- | Extract the list of applications from a term, a bit like a "multi-beta" reduction.

Some examples will help:
[(\x . t) a] -> Just ([(x, a)], t)

[[[(\x . (\y . (\z . t))) a] b] c] -> Just ([(x, a), (y, b), (z, c)]) t)

[[(\x . t) a] b] -> Nothing
-}
extractApps :: Term name uni fun a -> Maybe ([UTermDef name uni fun a], Term name uni fun a)
extractApps = go []
  where
    -- accumulate args in the argStack
    go argStack (Apply _ f arg) = go (arg : argStack) f
    -- no more applications, start matching args
    go argStack t               = matchArgs argStack [] t
    -- match the var declaration with the arg
    matchArgs (arg : rest) acc (LamAbs a n body) =
      matchArgs rest (Def (UVarDecl a n) arg : acc) body
    matchArgs [] acc t =
      if null acc then Nothing else Just (reverse acc, t)
    -- still args but term is not a lambda abstraction, returns nothing
    matchArgs (_ : _) _ _ = Nothing

-- | The inverse of 'extractApps'.
restoreApps :: [UTermDef name uni fun a] -> Term name uni fun a -> Term name uni fun a
restoreApps defs t = makeLams [] t (reverse defs)
  where
    makeLams args acc (Def (UVarDecl a n) rhs : rest) = makeLams (rhs : args) (LamAbs a n acc) rest
    makeLams args acc []                              = makeApps args acc
    -- This isn't the best annotation, but it will do
    makeApps (arg : args) acc = makeApps args (Apply (termAnn acc) acc arg)
    makeApps [] acc           = acc

-- | Run the inliner on a `UntypedPlutusCore.Core.Type.Term`.
processTerm ::
  forall name uni fun a.
  (InliningConstraints name uni fun) =>
  Term name uni fun a ->
  InlineM name uni fun a (Term name uni fun a)
processTerm =
  \case
    v@(Var _ n) -> do
      _ <- unsafePerformIO $ do
        putStrLn $ "Just a var"
        pure (pure ())
      fromMaybe v <$> substName n

    -- See Note [Differences from PIR inliner] 3
    (extractApps -> Just (bs, t)) -> do
      _ <- unsafePerformIO $ do
        putStrLn $ "extractApps"
        pure (pure ())
      bs' <- wither (processSingleBinding t) bs
      t' <- processTerm t
      pure $ restoreApps bs' t'

    t -> do
      -- See note [Processing order of call site inlining]
          let (hd, args) = UPLC.splitApplication t
          case args of
              -- not really an application, so hd is the term itself. Processing it will loop.
              [] -> do
                _ <- unsafePerformIO $ do
                  putStrLn $ "Not an application, args is empty"
                  pure (pure ())
                forMOf termSubterms t processTerm
              _ -> do
                  hd' <- processTerm hd
                  processedArgs <- forM (fmap snd args) processTerm
                  let
                    processedArgsWithAnn = zip (fmap fst args) processedArgs
                    reconstructed = mkIterApp hd' processedArgsWithAnn
                  case hd' of
                      Var _ name -> do
                          gets (lookupVarInfo name) >>= \case
                              Just varInfo -> do
                                let
                                  defAsInlineTerm = varInfo ^. varRhs
                                  inlineTermToTerm :: InlineTerm name uni fun ann
                                    -> Term name uni fun ann
                                  inlineTermToTerm (Done (Dupable var)) = var
                                  -- extract out the rhs without renaming, we only rename when we
                                  -- know there's substitution
                                  rhs = inlineTermToTerm defAsInlineTerm
                                maybeInlined <-
                                    callSiteInline
                                        (termSize reconstructed)
                                        rhs
                                        processedArgsWithAnn
                                let
                                        coercedT :: Term Name DefaultUni DefaultFun () = unsafeCoerce t
                                        coercedRe :: Term Name DefaultUni DefaultFun () = unsafeCoerce reconstructed

                                case maybeInlined of
                                  Just inlined -> do
                                    _ <- unsafePerformIO $ do
                                      let coercedInlined :: Term Name DefaultUni DefaultFun () = unsafeCoerce inlined
                                      putStrLn $ "inlined the term " <> display coercedT
                                      putStrLn $ "reconstructed term " <> display coercedRe
                                      putStrLn $ "inlined term " <> display coercedInlined
                                      pure (pure ())
                                    pure (fromMaybe reconstructed maybeInlined)
                                  Nothing -> do
                                    unsafePerformIO $ do
                                      putStrLn $ "Did NOT inline the term " <> display coercedT
                                      putStrLn $ "originalTerm = " <> display coercedRe
                                      pure (pure ())
                                    pure (fromMaybe reconstructed maybeInlined)
                              Nothing -> pure reconstructed
                      _ -> do
                        _ <- unsafePerformIO $ do
                          putStrLn $ "hd' is Not a var in the map "
                          pure (pure ())
                        pure reconstructed
  where
    -- See Note [Renaming strategy]
    substName :: name -> InlineM name uni fun a (Maybe (Term name uni fun a))
    substName name = gets (lookupTerm name) >>= traverse renameTerm
    -- See Note [Inlining approach and 'Secrets of the GHC Inliner']
    renameTerm :: InlineTerm name uni fun a -> InlineM name uni fun a (Term name uni fun a)
    renameTerm = \case
      -- Already processed term, just rename and put it in, don't do any
      -- further optimization here.
      Done t -> liftDupable t

processSingleBinding ::
  (InliningConstraints name uni fun) =>
  Term name uni fun a ->
  UTermDef name uni fun a ->
  InlineM name uni fun a (Maybe (UTermDef name uni fun a))
processSingleBinding body (Def vd@(UVarDecl a n) rhs0) = do
  maybeAddSubst body a n rhs0 >>= \case
    Just rhs -> do
      modify' . extendVarInfo n $ VarInfo { _varRhs = Done (dupable rhs)}
      pure . Just $ Def vd rhs
    Nothing -> pure Nothing

{- | Check against the heuristics we have for inlining and either inline the term binding or not.
The arguments to this function are the fields of the `TermBinding` being processed.
Nothing means that we are inlining the term:
  * we have extended the substitution, and
  * we are removing the binding (hence we return Nothing).
-}
maybeAddSubst ::
  forall name uni fun a.
  (InliningConstraints name uni fun) =>
  Term name uni fun a ->
  a ->
  name ->
  Term name uni fun a ->
  InlineM name uni fun a (Maybe (Term name uni fun a))
maybeAddSubst body a n rhs0 = do
  rhs <- processTerm rhs0

  -- Check whether we've been told specifically to inline this
  hints <- asks _hints
  let hinted = shouldInline hints a n

  if hinted -- if we've been told specifically, then do it right away
    then extendAndDrop (Done $ dupable rhs)
    else
      ifM
        (shouldUnconditionallyInline n rhs body)
        (extendAndDrop (Done $ dupable rhs))
        (pure $ Just rhs)
  where
    extendAndDrop ::
      forall b.
      InlineTerm name uni fun a ->
      InlineM name uni fun a (Maybe b)
    extendAndDrop t = modify' (extendTerm n t) >> pure Nothing

shouldUnconditionallyInline ::
  (InliningConstraints name uni fun) =>
  name ->
  Term name uni fun a ->
  Term name uni fun a ->
  InlineM name uni fun a Bool
shouldUnconditionallyInline n rhs body = do
  isTermPure <- checkPurity rhs
  preUnconditional isTermPure ||^ postUnconditional isTermPure
  where
    -- similar to the paper, preUnconditional inlining checks that the binder is 'OnceSafe'.
    -- I.e., it's used at most once AND it neither duplicate code or work.
    -- While we don't check for lambda etc like in the paper, `effectSafe` ensures that it
    -- isn't doing any substantial work.
    -- We actually also inline 'Dead' binders (i.e., remove dead code) here.
    preUnconditional isTermPure = nameUsedAtMostOnce n &&^ effectSafe body n isTermPure
    -- See Note [Inlining approach and 'Secrets of the GHC Inliner'] and [Inlining and
    -- purity]. This is the case where we don't know that the number of occurrences is
    -- exactly one, so there's no point checking if the term is immediately evaluated.
    postUnconditional isTermPure = pure isTermPure &&^ acceptable rhs

-- | Check if term is pure. See Note [Inlining and purity]
checkPurity :: Term name uni fun a -> InlineM name uni fun a Bool
checkPurity t = pure $ isPure t

nameUsedAtMostOnce ::
  forall name uni fun a.
  (InliningConstraints name uni fun) =>
  name ->
  InlineM name uni fun a Bool
nameUsedAtMostOnce n = do
  usgs <- asks _usages
  -- 'inlining' terms used 0 times is a cheap way to remove dead code while we're here
  pure $ Usages.getUsageCount n usgs <= 1

effectSafe ::
  forall name uni fun a.
  (InliningConstraints name uni fun) =>
  Term name uni fun a ->
  name ->
  -- | is it pure?
  Bool ->
  InlineM name uni fun a Bool
effectSafe body n purity = do
  -- This can in the worst case traverse a lot of the term, which could lead to us
  -- doing ~quadratic work as we process the program. However in practice most term
  -- types will make it give up, so it's not too bad.
  let immediatelyEvaluated = case firstEffectfulTerm body of
        Just (Var _ n') -> n == n'
        _               -> False
  pure $ purity || immediatelyEvaluated

{- | Should we inline? Should only inline things that won't duplicate work or code.
See Note [Inlining approach and 'Secrets of the GHC Inliner']
-}
acceptable :: Term name uni fun a -> InlineM name uni fun a Bool
acceptable t =
  -- See Note [Inlining criteria]
  pure $ costIsAcceptable t && sizeIsAcceptable t

{- |
Try to identify the first sub term which will be evaluated in the given term and
which could have an effect. 'Nothing' indicates that there's no term to evaluate.
-}
firstEffectfulTerm :: Term name uni fun a -> Maybe (Term name uni fun a)
firstEffectfulTerm = goTerm
  where
    goTerm = \case
      Apply _ fun args -> goTerm fun <|> goTerm args
      Force _ t        -> goTerm t
      Constr _ _ []    -> Nothing
      Constr _ _ ts    -> asum $ goTerm <$> ts
      Case _ t _       -> goTerm t
      t@Var{}          -> Just t
      t@Error{}        -> Just t
      Builtin{}        -> Nothing
      LamAbs{}         -> Nothing
      Constant{}       -> Nothing
      Delay{}          -> Nothing

{- | Is the cost increase (in terms of evaluation work) of inlining a variable whose RHS is
the given term acceptable?
-}
costIsAcceptable :: Term name uni fun a -> Bool
costIsAcceptable = \case
  Builtin{} -> True
  Var{} -> True
  Constant{} -> True
  Error{} -> True
  -- This will mean that we create closures at each use site instead of
  -- once, but that's a very low cost which we're okay rounding to 0.
  LamAbs{} -> True
  Apply{} -> False
  -- Inlining constructors of size 1 or 0 seems okay, but does result in doing
  -- the work for the elements at each use site.
  Constr _ _ es -> case es of
    []  -> True
    [e] -> costIsAcceptable e
    _   -> False
  -- Inlining a case means redoing the match at each use site
  Case{} -> False
  Force{} -> False
  Delay{} -> True

{- | Is the size increase (in the AST) of inlining a variable whose RHS is
the given term acceptable?
-}
sizeIsAcceptable :: Term name uni fun a -> Bool
sizeIsAcceptable = \case
  Builtin{} -> True
  Var{} -> True
  Error{} -> True
  -- See Note [Differences from PIR inliner] 4
  LamAbs{} -> False
  -- Inlining constructors of size 1 or 0 seems okay
  Constr _ _ es -> case es of
    []  -> True
    [e] -> sizeIsAcceptable e
    _   -> False
  -- Cases are pretty big, due to the case branches
  Case{} -> False
  -- Constants can be big! We could check the size here and inline if they're
  -- small, but probably not worth it
  Constant{} -> False
  Apply{} -> False
  Force _ t -> sizeIsAcceptable t
  Delay _ t -> sizeIsAcceptable t

isPure :: Term name uni fun a -> Bool
isPure = go True
  where
    -- See Note [delayAndVarIsPure]
    go delayAndVarIsPure = \case
      Var{}                         -> delayAndVarIsPure
      -- These are syntactically values that won't reduce further
      LamAbs{}                      -> True
      Constant{}                    -> True
      Delay _ body                  -> delayAndVarIsPure || go delayAndVarIsPure body
      -- This case is not needed in PIR's `isPure`, because PIR's beta-reduction pass
      -- turns terms like this into `Let` bindings.
      Apply _ (LamAbs _ _ body) arg -> go True arg && go delayAndVarIsPure body
      -- Applications can do work
      Apply{}                       -> False
      Force _ body                  -> go False body
      -- A constructor is pure if all of its elements are pure
      Constr _ _ es                 -> all (go True) es
      -- A case will compute the case branch, which could do anything
      Case{}                        -> False
      -- Error is obviously not pure
      Error{}                       -> False
      -- See Note [Differences from PIR inliner] 5
      Builtin{}                     -> True

-- | Apply and beta reduce, if possible.
applyAndBetaReduce ::
  forall name uni fun a.
  (InliningConstraints name uni fun) =>
  -- | The rhs of the variable, should have been renamed already
  Term name uni fun a ->
  -- | The arguments, already processed
  [(a, Term name uni fun a)] ->
  InlineM name uni fun a (Maybe (Term name uni fun a))
applyAndBetaReduce rhs args0 = do
  let go ::
        Term name uni fun a ->
        [(a, Term name uni fun a)] ->
        InlineM name uni fun a (Maybe (Term name uni fun a))
      go acc args = case (acc, args) of
        (LamAbs _ann n tm, (_, arg) : processedArgs) -> do
          safe <- safeToBetaReduce n arg acc
          if safe
            then do
              acc' <-
                termSubstNamesM
                  (\name -> if name == n then Just <$> PLC.rename arg else pure Nothing)
                  tm
              go acc' processedArgs
            -- if it is not safe to beta reduce, just return the processed application
            else pure . Just $ mkIterApp acc args
        -- no more lambda abstraction, just return the processed application
        (_, _) -> pure . Just $ mkIterApp acc args

      -- Is it safe to turn `(\a -> body) arg` into `body [a := arg]`?
      -- The criteria is the same as the criteria for unconditionally inlining `a`,
      -- since inlining is the same as beta reduction.
      safeToBetaReduce ::
        name ->
        Term name uni fun a ->
        Term name uni fun a ->
        InlineM name uni fun a Bool
      safeToBetaReduce = shouldUnconditionallyInline
  go rhs args0

{- | This works in the same way as `PlutusIR.Transform.Inline.CallSiteInline.callSiteInline`.
See Note [Inlining and beta reduction of functions].
-}
callSiteInline ::
  forall name uni fun a.
  (InliningConstraints name uni fun) =>
  -- | The term size if it were not inlined.
  Size ->
  -- | The Rhs of the "head" of the term, already processed.
  Term name uni fun a ->
  -- | The application context of the term, already processed.
  [(a, Term name uni fun a)] ->
  InlineM name uni fun a (Maybe (Term name uni fun a))
callSiteInline processedTSize headRhs = go
  where
    go :: [(a, Term name uni fun a)] ->
      InlineM name uni fun a (Maybe (Term name uni fun a))
    go args =
      do
        -- The definition itself will be inlined, so we need to check that the cost
        -- of that is acceptable. Note that we do _not_ check the cost of the _body_.
        -- We would have paid that regardless.
        -- Consider e.g. `let y = \x. f x`. We pay the cost of the `f x` at
        -- every call site regardless. The work that is being duplicated is
        -- the work for the lambda.
        let costIsOk = costIsAcceptable headRhs
        -- check if binding is pure to avoid duplicated effects.
        -- For strict bindings we can't accidentally make any effects happen less often
        -- than it would have before, but we can make it happen more often.
        -- We could potentially do this safely in non-conservative mode.
        rhsPure <- checkPurity headRhs
        if costIsOk && rhsPure then do
          -- rename the rhs of the variable before any substitution
          renamedRhs <- rename headRhs
          applyAndBetaReduce renamedRhs args >>= \case
            Just inlined -> do
              let -- Inline only if the size is no bigger than not inlining.
                  sizeIsOk = termSize inlined <= processedTSize
              pure $ if sizeIsOk then Just inlined else Nothing
            Nothing -> pure Nothing
        else pure Nothing

{- Note [delayAndVarIsPure]

To determine whether a `Term` is pure, we recurse into the `Term`. If we do not descend
into a `Force` node, then a `Var` node and a `Delay` node is always pure
(see Note [Purity, strictness, and variables] for why `Var` nodes are pure).

Once we descend into the body of a `Force` node, however, `Var` and `Delay` nodes can
no longer be considered unconditionally pure, because the following terms are impure:

```
force (delay impure)
force (force (delay (delay impure)))
force x  -- because `x` may expand into an delayed impure `Term`
force (force (delay x))
```

This is the purpose of the `delayAndVarIsPure` flag, which is set to `False` when
descending into the body of `Force`.

This can potentially be improved, e.g., it would consider the following pure terms
as impure:

```
force (delay (delay impure))
force (delay x)
```

However, since we can rely on `forceDelayCancel` to cancel adjacent `force` and `delay`
nodes, we do not need to be too clever here.
-}
