module PlutusIR.Transform.RewriteRules.Tests where

import PlutusCore.Quote
import PlutusCore.Test hiding (ppCatch)
import PlutusIR.Compiler qualified as PIR
import PlutusIR.Parser
import PlutusIR.Test
import PlutusIR.Transform.RewriteRules as RewriteRules
import PlutusPrelude

import Test.Tasty

test_RewriteRules :: TestTree
test_RewriteRules = runTestNestedIn ["plutus-ir/test/PlutusIR/Transform"] $
    testNested "RewriteRules"
    [ testNested "pir" $ fmap
        (goldenPir (runQuote . RewriteRules.rewriteWith def)  pTerm)
        [ "equalsInt" -- this tests that the function works on equalInteger
        , "divideInt" -- this tests that the function excludes not commutative functions
        , "multiplyInt" -- this tests that the function works on multiplyInteger
        , "let" -- this tests that it works in the subterms
        , "unConstrConstrDataFst"
        , "unConstrConstrDataSnd"
        ]
    , testNested "trace" $ fmap
        (goldenPirEvalTrace pTermAsProg)
        [ "unConstrConstrDataFst"
        ]
    ]

  where
    goldenPirEvalTrace = goldenPirM $ \ast -> ppCatch $ do
          -- we need traces to remain for checking the evaluation-order
          tplc <- asIfThrown $ compileWithOpts ( set (PIR.ccOpts . PIR.coPreserveLogging) True) ast
          runUPlcLogs [void tplc]
