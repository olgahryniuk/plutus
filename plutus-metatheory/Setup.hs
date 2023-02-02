{-# LANGUAGE ImportQualifiedPost #-}

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Distribution.Simple qualified as D
import Distribution.Simple.PreProcess qualified as D
import Distribution.Simple.Program qualified as D
import Distribution.Simple.Program.Types qualified as D
import Distribution.Simple.Utils as D
import Distribution.Types.BuildInfo qualified as D
import Distribution.Types.ComponentLocalBuildInfo qualified as D
import Distribution.Types.LocalBuildInfo qualified as D
import Distribution.Verbosity qualified as D

import System.IO.Unsafe (unsafePerformIO)

{-
When we run cabal build, we want cabal to detect changes to the *.lagda files.
If there were changes, we want cabal to invoke agda under the hood, and place the
generated haskell modules inside dist-newstyle. We also want to expose those very
haskell modules in plutus-metatheory.cabal, so that they are available to the
executables and test suites.

This is similar to what Alex & Happy do. For example with Alex, we might have a
Tokens.x file, which will be turned into a Tokens.hs module during compilation.
In the cabal file, we can add "Tokens" to the exposed-modules, even though Tokens.hs
isn't part of the source tree, but is in fact autogenerated in dist-newstyle from
Tokens.x!

In order to achieve this, cabal PreProcessors are used. First we make sure to list
src/**/*.lagda in the cabal's extra-source-files. This way, when changes to .lagda
files are made, cabal will pre-process them anew, with the pre-processor being run
once on each changed file.

With Alex, each .x file will be pre-processed and thus translated into a .hs file
with the same basename. However our setup is more complex than that. We don't have
that level of granularity in our choice of agda compilation. Instead, we invoke agda
on the entire source tree, and only once, using src/Main.ladga as our compilation target.

Again if more than one .lagda file was modified, and we run cabal build, we only need to
invoke agda once, with all subsequent invokations being noops, but still kind of slow.
This is why we use the agdaProgramStatus IORef: to cut down compilation times.

Finally, the order in which the modules are listed in exposed-modules matters a lot!
The MAlonzo.Code files must be listed last, otherwise cabal will fail with:
setup: can't find source for MAlonzo/Code/* in src

TODO Newer (> 3.6.3.0) versions of the cabal library introduced a ppOrdering field in
PreProcessor which can reorder all modules.
https://hackage.haskell.org/package/Cabal-3.8.1.0/docs/Distribution-Simple-PreProcess.html
Until we upgrade cabal, we just have to be careful to expose our modules in the right order.
Once cabal is upgraded, we can implement ppOrdering as reorderModules:

reorderModules :: Verbosity -> [FilePath] -> [ModuleName] -> IO [ModuleName]
reorderModules _ _ = sortBy malonzoCodeOrdering
  where
    malonzoCodeOrdering :: ModuleName -> ModuleName -> Ordering
    malonzoCodeOrdering name _
      | "MAlonzo.Code" `isPrefixOf` name = GT
      | otherwise = EQ
-}

data AgdaProgramStatus
  = Run
  | NotRun
  deriving (Eq, Ord, Show)


agdaProgramStatus :: IORef AgdaProgramStatus
agdaProgramStatus = unsafePerformIO (newIORef NotRun)
{-# NOINLINE agdaProgramStatus #-}


main :: IO ()
main = D.defaultMainWithHooks userHooks
  where
    userHooks :: D.UserHooks
    userHooks = D.simpleUserHooks { D.hookedPreProcessors = preProcessors }

    preProcessors :: [D.PPSuffixHandler]
    preProcessors = [("lagda", agdaPreProcessor)]


agdaPreProcessor :: D.BuildInfo -> D.LocalBuildInfo -> D.ComponentLocalBuildInfo -> D.PreProcessor
agdaPreProcessor _ lbi _ = D.PreProcessor
  { D.platformIndependent = True
  , D.runPreProcessor = D.mkSimplePreProcessor preProcessors
  }
  where
    preProcessors :: FilePath -> FilePath -> D.Verbosity -> IO ()
    preProcessors _ _ verb = do
      status <- readIORef agdaProgramStatus
      case status of
        NotRun -> do
          D.notice verb "***** running agda preprocessor..."
          runAgda verb
          writeIORef agdaProgramStatus Run
        Run ->
          D.notice verb "***** agda already run, skipping preprocessor hook"

    runAgda :: D.Verbosity -> IO ()
    runAgda verb = D.runProgram verb agdaProgram
      [ "--compile-dir", D.buildDir lbi
      , "--compile"
      , "--ghc-dont-call-ghc"
      , "--local-interfaces"
      , "src/Main.lagda"
      ]

    agdaProgram :: D.ConfiguredProgram
    agdaProgram = D.simpleConfiguredProgram "agda" (D.FoundOnSystem "agda")
