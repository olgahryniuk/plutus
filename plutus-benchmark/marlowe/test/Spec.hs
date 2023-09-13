-- Budget tests for Marlowe scripts
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.List qualified as List
import Test.Tasty
import Test.Tasty.Extras

import PlutusBenchmark.Marlowe.BenchUtil (benchmarkToUPLC, rolePayoutBenchmarks,
                                          semanticsBenchmarks)
import PlutusBenchmark.Marlowe.Scripts.RolePayout (rolePayoutValidator)
import PlutusBenchmark.Marlowe.Scripts.Semantics (marloweValidator)
import PlutusBenchmark.Marlowe.Types qualified as M
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Test (goldenUEval, goldenUEvalCatch, goldenUEvalLogs, goldenUplcBudget)
import PlutusCore.Version qualified as PLC
import PlutusLedgerApi.V2 (scriptContextTxInfo, txInfoId)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Test
import UntypedPlutusCore (NamedDeBruijn)
import UntypedPlutusCore.Core.Type qualified as UPLC

mkBudgetTest ::
    CompiledCode a
    -> M.Benchmark
    -> (String, UPLC.Term NamedDeBruijn DefaultUni DefaultFun ())
mkBudgetTest validator bm@M.Benchmark{..} =
  let benchName = show $ txInfoId $ scriptContextTxInfo bScriptContext
  in
    (benchName, benchmarkToUPLC validator bm )

main :: IO ()
main = do

  -- Read the semantics benchmark files.
  semanticsMBench <- either error id <$> semanticsBenchmarks

  -- Read the role payout benchmark files.
  rolePayoutMBench <- either error id <$> rolePayoutBenchmarks

  let allTests :: TestTree
      allTests =
        testGroup "plutus-benchmark Marlowe tests"
            [ runTestNestedIn ["marlowe", "test"] $ testNested "semantics"
              [ testNested "budget" $ map (uncurry goldenUplcBudget . mkBudgetTest marloweValidator) semanticsMBench
              , testNested "evaluation" $ map (uncurry goldenUEvalLogs . fmap List.singleton . fmap (UPLC.Program () PLC.latestVersion) . mkBudgetTest marloweValidator) semanticsMBench
              ]
            , runTestNestedIn ["marlowe", "test"] $ testNested "role-payout" $
              [ testNested "budget" $ map (uncurry goldenUplcBudget . mkBudgetTest rolePayoutValidator) rolePayoutMBench
              , testNested "evaluation" $ map (uncurry goldenUEval . fmap List.singleton . mkBudgetTest rolePayoutValidator) rolePayoutMBench
              ]
            --, runTestNestedIn ["marlowe", "test"] $ testNested "role-payout" $ testNested "evaluation" $
            ]
  defaultMain allTests
