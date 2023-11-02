-- editorconfig-checker-disable-file
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-pir=0 #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-uplc=0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:context-level=0 #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-compilation-trace #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module IsData.Spec where

import Test.Tasty.Extras

import Plugin.Data.Spec

import PlutusCore.Test
import PlutusTx qualified as Tx
import PlutusTx.AsData qualified as AsData
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Code
import PlutusTx.DataMap
import PlutusTx.IsData qualified as IsData
import PlutusTx.Plugin
import PlutusTx.Prelude qualified as P
import PlutusTx.Test

import PlutusCore qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import UntypedPlutusCore qualified as UPLC

import Data.Proxy


fieldAccessor :: CompiledCode (Tx.BuiltinData -> Integer)
fieldAccessor = plc (Proxy @"fieldAccessor")
  (\inp -> let r :: Foo = Tx.unsafeFromBuiltinData inp in xxxxx r `Builtins.addInteger` yyyyy r `Builtins.addInteger` aaa r `Builtins.addInteger` bbb r)

fieldAccessor2 :: CompiledCode (Tx.BuiltinData ->  Integer)
fieldAccessor2 = plc (Proxy @"fieldAccessor2")
  (\inp -> let (Bar a b _ c d) = Tx.unsafeFromBuiltinData inp in a `Builtins.addInteger` b `Builtins.addInteger` c `Builtins.addInteger` d)

fieldAccessor3 :: CompiledCode (Tx.BuiltinData -> Integer)
fieldAccessor3 = plc (Proxy @"fieldAccessor3")
  (\inp -> let r :: Snooker = Tx.unsafeFromBuiltinData inp in myx r `Builtins.addInteger` myy r)

tests :: TestNested
tests = testNestedGhc "IsData" [
    goldenPirReadable "fieldAccessorFoo" fieldAccessor,
    goldenPirReadable "fieldAccessorFoo2" fieldAccessor2,
    goldenPirReadable "fieldAccessorFoo3" fieldAccessor3,
    goldenEvalCekCatch "fieldAccessorFoo" $ [fieldAccessor `unsafeApplyCode` foo],
    goldenEvalCekCatch "fieldAccessorFoo2" $ [fieldAccessor2 `unsafeApplyCode` foo],
    -- goldenEvalCekCatch "fieldAccessorFoo3" $ [fieldAccessor3 `unsafeApplyCode` foo],
    goldenBudget "fieldAccessorFoo-budget" (fieldAccessor `unsafeApplyCode` foo),
    goldenBudget "fieldAccessorFoo2-budget" (fieldAccessor2 `unsafeApplyCode` foo)
    -- goldenBudget "fieldAccessorFoo3-budget" (fieldAccessor3 `unsafeApplyCode` foo)
  ]

foo :: CompiledCode Tx.BuiltinData
foo = Tx.liftCodeDef (Tx.toBuiltinData (Bar 42 147 "Hello" 1000 10000))
