-- editorconfig-checker-disable-file
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

-- | Various analyses of events in mainnet script dumps.
-- This only deals with PlutusV1 and PlutusV2 script events because
-- PlutusLedgerApi.Test.EvaluationEvent (and hence the scriptdump job) doesn't
-- know about anything else yet.

module Main (main) where

import LoadScriptEvents (eventsOf, loadEvents)

import PlutusCore.Data as Data (Data (..))
import PlutusCore.Default (DefaultUni (DefaultUniData), Some (..), ValueOf (..))
import PlutusCore.Evaluation.Machine.CostStream (sumCostStream)
import PlutusCore.Evaluation.Machine.ExMemoryUsage (ExMemoryUsage, flattenCostRose, memoryUsage)
import PlutusCore.Pretty (display)
import PlutusLedgerApi.Common
import PlutusLedgerApi.Test.EvaluationEvent
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx.AssocMap qualified as M
import UntypedPlutusCore as UPLC

import Control.Lens hiding (List)
import Control.Monad.Writer.Strict
import Data.List (intercalate)
import Data.SatInt (fromSatInt)
import System.Directory.Extra (listFiles)
import System.Environment (getArgs, getProgName)
import System.FilePath (isExtensionOf)
import System.IO (stderr)
import Text.Printf (hPrintf, printf)

-- | The type of a generic analysis function
type EventAnalyser
    =  EvaluationContext
    -> [Integer]  -- cost parameters
    -> ScriptEvaluationEvent
    -> IO ()

-- Analyse values in ScriptContext

-- Script purpose: same for V1 and V2, but changes in V3
stringOfPurposeV1 :: V1.ScriptPurpose -> String
stringOfPurposeV1 = \case
    V1.Minting    _ -> "V1 Minting"      -- Script arguments are [redeemer, context]
    V1.Spending   _ -> "V1 Spending"     -- Script arguments are [datum, redeemer, context]
    V1.Rewarding  _ -> "V1 Rewarding"    -- Script arguments are [datum, redeemer, context]
    V1.Certifying _ -> "V1 Certifying"   -- Script arguments are [redeemer, context]?

stringOfPurposeV2 :: V2.ScriptPurpose -> String
stringOfPurposeV2 = \case
    V2.Minting    _ -> "V2 Minting"
    V2.Spending   _ -> "V2 Spending"
    V2.Rewarding  _ -> "V2 Rewarding"
    V2.Certifying _ -> "V2 Certifying"

shapeOfValue :: V1.Value  -> String
shapeOfValue (V1.Value m) =
    let l = M.toList m
    in printf "[%d: %s]" (length l) (intercalate "," (fmap (printf "%d" . length . M.toList . snd) l))

analyseValue :: V1.Value -> IO ()
analyseValue v = do
  putStr $ shapeOfValue v
  printf "\n"

analyseTxInfoV1 :: V1.TxInfo -> IO ()
analyseTxInfoV1 i = do
  putStr "Fee:     "
  analyseValue $ V1.txInfoFee i
  putStr "Mint:    "
  analyseValue $ V1.txInfoMint i
  case V1.txInfoOutputs i of
    [] -> putStrLn "No outputs"
    l  -> do
      putStr $ printf "Outputs %d " (length l)
      putStrLn $ intercalate ", " (fmap (shapeOfValue . V1.txOutValue) l)

analyseTxInfoV2 :: V2.TxInfo -> IO ()
analyseTxInfoV2 i = do
  putStr "Fee:     "
  analyseValue $ V2.txInfoFee i
  putStr "Mint:    "
  analyseValue $ V2.txInfoMint i
  case V2.txInfoOutputs i of
    [] -> putStrLn "No outputs"
    l  -> do
      putStr $ printf "Outputs %d " (length l)
      putStrLn $ intercalate ", " (fmap (shapeOfValue . V2.txOutValue) l)

analyseScriptContext :: EventAnalyser
analyseScriptContext _ctx _params ev = case ev of
    PlutusV1Event ScriptEvaluationData{..} _expected ->
        case dataInputs of
        [_,_,c] -> analyseCtxV1 c
        [_,c]   -> analyseCtxV1 c
        l       -> error $ printf "Unexpected number of V1 script arguments: %d" (length l)
    PlutusV2Event ScriptEvaluationData{..} _expected ->
        case dataInputs of
        [_,_,c] -> analyseCtxV2 c
        [_,c]   -> analyseCtxV2 c
        l       -> error $ printf "Unexpected number of V2 script arguments: %d" (length l)
    where
    analyseCtxV1 c = case V1.fromData @V1.ScriptContext c of
                       Nothing -> error "Failed to decode V1 ScriptContext"
                       Just p -> do
                         putStrLn "----------------"
                         putStrLn $ stringOfPurposeV1 $ V1.scriptContextPurpose p
                         analyseTxInfoV1 $ V1.scriptContextTxInfo p
    analyseCtxV2 c = case V2.fromData @V2.ScriptContext c of
                       Nothing -> error "Failed to decode V2 ScriptContext"
                       Just p -> do
                         putStrLn "----------------"
                         putStrLn $ stringOfPurposeV2 $ V2.scriptContextPurpose p
                         analyseTxInfoV2 $ V2.scriptContextTxInfo p

-- Data object analysis

-- Statistics about a Data object

data DataInfo = DataInfo
    { _memUsage   :: Integer
    , _numNodes   :: Integer
    , _depth      :: Integer
    , _numInodes  :: Integer
    , _maxIsize   :: Integer  -- Maximum memoryUsage of integers in I nodes
    , _totalIsize :: Integer  -- Total memoryUsage of integers in I nodes
    , _numBnodes  :: Integer
    , _maxBsize   :: Integer  -- Maximum memoryUsage of bytestrings in B nodes
    , _totalBsize :: Integer  -- Total memoryUsage of bytestrings in B nodes
    , _numLnodes  :: Integer
    , _maxLlen    :: Integer  -- Maximum list length
    , _numCnodes  :: Integer
    , _maxClen    :: Integer  -- Maximum number of constructor arguments
    , _numMnodes  :: Integer
    , _maxMlen    :: Integer  -- Maximum map length
    } deriving stock (Show)
makeLenses ''DataInfo

emptyInfo :: DataInfo
emptyInfo = DataInfo 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- Memory usage as an Integer (in units of 64 bits / 8 bytes)
memU :: ExMemoryUsage a => a -> Integer
memU = fromSatInt . sumCostStream . flattenCostRose . memoryUsage

-- Header (useful for R)
printDataHeader :: IO ()
printDataHeader =
    printf "memUsage numNodes depth numI maxIsize totalIsize numB maxBsize totalBsize numL maxL numC maxC numM maxM\n"

printDataInfo :: DataInfo -> IO ()
printDataInfo DataInfo{..} =
    printf "%4d %4d %4d    %4d %4d %4d    %4d %4d %4d    %4d %4d   %4d %4d    %4d %4d\n"
           _memUsage _numNodes _depth
           _numInodes _maxIsize _totalIsize
           _numBnodes _maxBsize _totalBsize
           _numLnodes _maxLlen
           _numCnodes _maxClen
           _numMnodes _maxMlen

-- Traverse a Data object collecting information
getDataInfo :: Data -> DataInfo
getDataInfo d =
    let ilen = fromIntegral . length
        info = go d emptyInfo
        go x i =
            case x of
              I n             -> i & numInodes +~ 1 & maxIsize %~ max s & totalIsize +~ s where s = memU n
              B b             -> i & numBnodes +~ 1 & maxBsize %~ max s & totalBsize +~ s where s = memU b
              List l          -> foldr go i' l where i' = i & numLnodes +~ 1 & maxLlen %~ max (ilen l)
              Data.Constr _ l -> foldr go i' l where i' = i & numCnodes %~ (+1) & maxClen %~ max (ilen l)
              Map l           -> let (a,b) = unzip l
                                 in foldr go (foldr go i' a) b where i' = i & numMnodes +~ 1 & maxMlen %~ max (ilen l)
        getDepth = \case
              I _             -> 1
              B _             -> 1
              List l          -> 1 + depthList l
              Data.Constr _ l -> 1 + depthList l
              Map l           -> let (a,b) = unzip l
                                 in 1 + max (depthList a) (depthList b)
        depthList = foldl (\n a -> max n (getDepth a)) 0
        totalNodes = sum $ info ^.. (numInodes <> numBnodes <> numLnodes <> numCnodes <> numMnodes)
    in info & memUsage .~ memU d & numNodes .~ totalNodes & depth .~ getDepth d

printDataInfoFor :: Data -> IO ()
printDataInfoFor = printDataInfo <$> getDataInfo


-- Analyse a redeemer (as a Data object) from a script evaluation event
analyseRedeemer :: EventAnalyser
analyseRedeemer _ctx _params ev = do
  case ev of
      PlutusV1Event ScriptEvaluationData{..} _expected ->
          case dataInputs of
            [_d, r,_c] -> printDataInfoFor r
            [r,_c]     -> printDataInfoFor r
            _          -> putStrLn "Unexpected script arguments"
      PlutusV2Event ScriptEvaluationData{..} _expected ->
          case dataInputs of
            [_d, r,_c] -> printDataInfoFor r
            [r,_c]     -> printDataInfoFor r
            _          -> putStrLn "Unexpected script arguments"

-- Analyse a datum (as a Data object) from a script evaluation event
analyseDatum :: EventAnalyser
analyseDatum _ctx _params ev = do
  case ev of
      PlutusV1Event ScriptEvaluationData{..} _expected ->
          case dataInputs of
            [d, _r,_c] -> printDataInfoFor d
            [_r,_c]    -> pure ()
            _          -> putStrLn "Unexpected script arguments"
      PlutusV2Event ScriptEvaluationData{..} _expected ->
          case dataInputs of
            [_d, r,_c] -> printDataInfoFor r
            [_r,_c]    -> pure ()
            _          -> putStrLn "Unexpected script arguments"

-- Extract the script from an evaluation event and apply some analysis function
analyseUnappliedScript
    :: (Term NamedDeBruijn DefaultUni DefaultFun () -> IO ())
    -> EventAnalyser
analyseUnappliedScript analyse _ctx _params ev = do
  case ev of
      PlutusV1Event ScriptEvaluationData{..} _expected ->
          go $ deserialiseScript PlutusV1 dataProtocolVersion dataScript
      PlutusV2Event ScriptEvaluationData{..} _expected ->
          go $ deserialiseScript PlutusV2 dataProtocolVersion dataScript
    where go = \case
               Left err -> putStrLn $ show err
               Right s ->
                   let ScriptNamedDeBruijn (Program _ _ t) = deserialisedScript s
                   in analyse t

-- Print statistics about Data objects in a Term
analyseTermDataObjects :: Term NamedDeBruijn DefaultUni DefaultFun () -> IO ()
analyseTermDataObjects = go
    where go =
              \case
               Var _ _            -> pure ()
               LamAbs _ _ t       -> go t
               Apply _ t1 t2      -> go t1 >> go t2
               Force _ t          -> go t
               Delay _ t          -> go t
               Constant _ c       ->
                   case c of
                     Some (ValueOf DefaultUniData d) -> printDataInfoFor d
                     _                               -> pure ()
               Builtin _ _        -> pure ()
               Error _            -> pure ()
               UPLC.Constr _ _ ts -> mapM_ go ts
               Case _ t1 t2       -> go t1 >> mapM_ go t2

-- | Run some analysis function over the events from a single event dump file
analyseOneFile
    :: EventAnalyser
    -> FilePath
    -> IO ()
analyseOneFile analyse eventFile = do
  events <- loadEvents eventFile
  case ( mkContext V1.mkEvaluationContext (eventsCostParamsV1 events)
       , mkContext V2.mkEvaluationContext (eventsCostParamsV2 events)
       ) of
    (Right ctxV1, Right ctxV2) ->
        mapM_ (runSingleEvent ctxV1 ctxV2) (eventsOf events)
    (Left err, _) -> error $ display err
    (_, Left err) -> error $ display err
  where
    mkContext f = \case
        Nothing         -> Right Nothing
        Just costParams -> Just . (,costParams) . fst <$> runWriterT (f costParams)

    runSingleEvent
        :: Maybe (EvaluationContext, [Integer])
        -> Maybe (EvaluationContext, [Integer])
        -> ScriptEvaluationEvent
        -> IO ()
    runSingleEvent ctxV1 ctxV2 event =
        case event of
          PlutusV1Event{} ->
              case ctxV1 of
                Just (ctx, params) -> analyse ctx params event
                Nothing            -> putStrLn "*** ctxV1 missing ***"
          PlutusV2Event{} ->
              case ctxV2 of
                Just (ctx, params) -> analyse ctx params event
                Nothing            -> putStrLn "*** ctxV2 missing ***"

main :: IO ()
main =
    let analyses =
            [ ("values",      doAnalysis analyseScriptContext)
            , ("redeemers" ,  printDataHeader `thenDoAnalysis` analyseRedeemer)
            , ("datums",      printDataHeader `thenDoAnalysis` analyseDatum)
            , ("script-data", printDataHeader `thenDoAnalysis` analyseUnappliedScript analyseTermDataObjects)
            ]

        doAnalysis analyser = mapM_ (analyseOneFile analyser)
        (prelude `thenDoAnalysis` analyser) files = prelude >> doAnalysis analyser files

        usage = do
          getProgName >>= hPrintf stderr "Usage: %s <dir> <analysis>\n"
          hPrintf stderr"Avaliable analyses:\n"
          mapM_ (hPrintf stderr "   %s\n") (fmap fst analyses)

    in getArgs >>= \case
           [dir, name] ->
               case lookup name analyses of
                    Nothing       -> printf "Unknown analysis: %s\n" name >> usage
                    Just analysis ->
                        filter ("event" `isExtensionOf`) <$> listFiles dir >>= \case
                                   []         -> printf "No event files in %s\n" dir
                                   eventFiles -> analysis eventFiles
           _ -> usage

