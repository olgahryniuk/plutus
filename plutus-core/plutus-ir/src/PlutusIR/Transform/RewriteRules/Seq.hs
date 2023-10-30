{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PlutusIR.Transform.RewriteRules.Seq
    ( seqP
    ) where

import Prelude hiding (seq)

import PlutusIR
import PlutusIR.Purity
import PlutusCore.Quote
import PlutusIR.Analysis.VarInfo
import PlutusIR.MkPir
import PlutusIR.Analysis.Builtins
import PlutusCore.Builtin

-- | A thin wrapper to optimize codegen by omitting call to `seq` if left operand `isPure`.
seqP :: ( MonadQuote m
       , Monoid a
       , ToBuiltinMeaning uni fun
       , t ~ Term tyname Name uni fun a
       )
     => BuiltinsInfo uni fun
     -> VarsInfo tyname Name uni a
     -> Type tyname uni a -- ^ the type of left operand a
     -> t -- ^ left operand a
     -> t -- ^ right operand b
     -> m t
seqP binfo vinfo _ a b | isPure binfo vinfo a = pure b
seqP _ _ aT a b = seq aT a b

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
-}
seq :: (MonadQuote m, t ~ Term tyname Name uni fun a, Monoid a)
     => Type tyname uni a -> t -> t -> m t
seq aT a b = do
    -- There is an alternative implementation which constructs an immediate-applied lambda, the
    -- benefit of it being its compatibility to (u)plc (aka `TermLike`).
    -- MAYBE: This could be moved to PlutusCore.StdLib if it is written in immediate-lam style;
    -- however, because StdLib lives outside of MonadQuote, it would break global-uniqueness.
    wildVar <- freshName "wild"
    pure $ mkLet mempty NonRec [TermBind mempty Strict (VarDecl mempty wildVar aT) a] b
