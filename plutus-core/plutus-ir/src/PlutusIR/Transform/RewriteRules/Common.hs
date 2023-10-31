{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module PlutusIR.Transform.RewriteRules.Common
    ( seqA
    , seqPure
    , seq
    , strictLet
    , pattern A
    , pattern B
    , pattern I
    ) where

import Prelude hiding (seq)

import PlutusCore.Builtin
import PlutusCore.Quote
import PlutusIR
import PlutusIR.Analysis.Builtins
import PlutusIR.Analysis.VarInfo
import PlutusIR.MkPir
import PlutusIR.Purity

{- | Another wrapper that can be more easily turned into an infix operator

e.g. `infixr 0 (***) = seqA binfo vInfo`
-}
seqA :: (MonadQuote m, Monoid a, ToBuiltinMeaning uni fun)
     => BuiltinsInfo uni fun
     -> VarsInfo tyname Name uni a
     -> (Type tyname uni a, Term tyname Name uni fun a)
     ->  m (Term tyname Name uni fun a)
     -> m (Term tyname Name uni fun a)
seqA binfo vinfo (a,aT) y = seqPure binfo vinfo a aT <*> y

-- | A thin wrapper to optimize codegen by omitting call to `seq` if left operand `isPure`.
seqPure :: ( MonadQuote m
       , Monoid a
       , ToBuiltinMeaning uni fun
       , t ~ Term tyname Name uni fun a
       )
     => BuiltinsInfo uni fun
     -> VarsInfo tyname Name uni a
     -> Type tyname uni a -- ^ the type of left operand a
     -> t -- ^ left operand a
     -> m (t -> t) -- ^ how to modify right operand b
seqPure binfo vinfo aT a | isPure binfo vinfo a = pure id
                         | otherwise = seq aT a

{- | Takes as input two arbitrary PIR terms `a` and `b`,
and composes them to a single PIR term that, when executed,
would strictly evaluate the first term `a` only for its effects (i.e. ignoring its result)
while returning the result of the second term `b`.

The name is intentionally taken from Haskell's `GHC.Prim.seq`: they look
similar, but note that Haskell is lazy whereas PIR is strict, which means that
the result `b` will also be strictly evaluated in PIR.

The need of this `seq` "combinator" is when rewriting to more efficient code while trying to retain
the effects (that would otherwise be lost if it that code was considered completely dead).

Unfortunately, unlike Haskell's `seq`, we need the pass the correct `Type` of `a`,
so as to apply this `seq` combinator. This is usually not a problem because we are generating
code and we should have the type of `a` somewhere available.

There is an alternative implementation which constructs an immediate-applied lambda, the
benefit of it being its compatibility to (u)plc (aka `TermLike`).
MAYBE: This could be moved to PlutusCore.StdLib if it is written in immediate-lam style;
however, because StdLib lives outside of MonadQuote, it would break global-uniqueness.
-}
seq :: (MonadQuote m, Monoid a, t ~ Term tyname Name uni fun a)
    => Type tyname uni a -- ^ the type of left operand a
    -> t  -- ^ left operand a
    -> m (t -> t) -- ^ how to modify right operand b
seq aT a = snd <$> strictLet aT a

{- | A helper to create a strict binding; It returns the bound `Var`iable and
a function `Term -> Term`, expecting an "in-Term" to form a let-expression.
-}
strictLet :: (MonadQuote m, t ~ Term tyname Name uni fun a, Monoid a)
    => Type tyname uni a -- ^ the type of left operand a
    -> t -- ^ left  operand a
    -> m (t, t -> t) -- ^ the new Var and a function that takes the "in" part to construct the Let
strictLet aT a = do
    -- MAYBE: i wish this was less constrained to Name
    genName <- freshName "generated"
    pure (Var mempty genName, mkLet mempty NonRec [TermBind mempty Strict (VarDecl mempty genName aT) a])

-- Some shorthands for easier pattern-matching when creating rewrite rules
-- TODO: these patterns ignores annotations. Find a better way for easier writing rules that does
-- not ignore annotations e.g. (pattern-PIR-quasiquoters?)
pattern A :: Term tyname name uni fun a -> Term tyname name uni fun a -> Term tyname name uni fun a
pattern A l r <- Apply _ l r
pattern B :: fun -> Term tyname name uni fun a
pattern B b <- Builtin _ b
pattern I :: Term tyname name uni fun a -> Type tyname uni a -> Term tyname name uni fun a
pattern I e t <- TyInst _ e t
