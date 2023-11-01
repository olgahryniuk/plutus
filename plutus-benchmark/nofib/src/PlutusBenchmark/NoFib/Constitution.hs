{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module PlutusBenchmark.NoFib.Constitution where

import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (Bool (..), Integer, (&&), (<), (<=))
import PlutusTx.Prelude qualified as PlutusTx
import Prelude (Maybe (..), otherwise)

constitutionValidator :: () -> ScriptContext -> Bool
constitutionValidator _ sc = PlutusTx.all constitutional pps
  where
    txInfo = scriptContextTxInfo sc
    pps = txInfoProposalProcedures txInfo
    validRange = txInfoValidRange txInfo
    constitutional :: ProposalProcedure -> Bool
    constitutional (ProposalProcedure _ _ govAction) = case govAction of
      ParameterChange _ (ChangedParameters d) ->
        let parameters :: Map BuiltinByteString Integer = PlutusTx.unsafeFromBuiltinData d
         in if
              | Just maxBlockSize <- Map.lookup "maxBlockBodySize" parameters ->
                  0 < maxBlockSize && maxBlockSize < 100000
              | Just collateralPercentage <- Map.lookup "collateralPercentage" parameters ->
                  0 < collateralPercentage
              | Just govActionLifetime <- Map.lookup "govActionLifetime" parameters ->
                  1 <= govActionLifetime && govActionLifetime <= 15
              | otherwise -> True
      TreasuryWithdrawals{} -> from 123456789 `contains` validRange
      _ -> True

constitutionScript :: PlutusTx.CompiledCode (() -> ScriptContext -> Bool)
constitutionScript = $$(PlutusTx.compile [||constitutionValidator||])
