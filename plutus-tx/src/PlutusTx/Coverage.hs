{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module PlutusTx.Coverage ( CoverageAnnotation(..)
                         , CoverageIndex(..)
                         , CoverageMetadata(..)
                         , Metadata(..)
                         , CoverageData(..)
                         , CoverageReport(..)
                         , CovLoc(..)
                         , covLocFile
                         , covLocStartLine
                         , covLocEndLine
                         , covLocStartCol
                         , covLocEndCol
                         , metadataSet
                         , coverageAnnotations
                         , ignoredAnnotations
                         , coverageMetadata
                         , coveredAnnotations
                         , addLocationToCoverageIndex
                         , addBoolCaseToCoverageIndex
                         , coverageDataFromLogMsg
                         , pprCoverageReport
                         ) where

import Control.Lens

import Codec.Serialise

import PlutusCore.Flat

import Control.DeepSeq
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Text.Read

import Control.Monad.Writer

import Prettyprinter

import Prelude

import Flat hiding (to)

{- Note [Coverage annotations]
   During compilation we can insert coverage annotations in `trace` calls in
   the PIR code that are tracked in the relevant downstream code by looking at
   the logs. For example, the `LocationCoverage` option piggy-backs on the GHC
   source location annotations (in the form of `Tick`s) and insert `CoverageAnnLocation`
   annotations in the PIR that match the source location in the `Tick`.
-}

{- Note [Adding more coverage annotations]
   To add more coverage annotations we need to:
   1. Add a constructor to `CoverageType`
   2. Add option parsing to the function `parsePluginArgs`
   3. Add a coverage annotation to `CoverageAnn`
   4. Make any necessary modifications to `CoverageIndex`
   5. Add code to the compiler (e.g. in `compileExpr`) to insert
      the `CoverageAnn` and meta-data in the index
-}

-- | A source location for coverage
data CovLoc = CovLoc { _covLocFile      :: String
                     , _covLocStartLine :: Int
                     , _covLocEndLine   :: Int
                     , _covLocStartCol  :: Int
                     , _covLocEndCol    :: Int }
  deriving (Ord, Eq, Show, Read, Generic, Serialise)
  deriving Flat via (AsSerialize CovLoc)
  deriving anyclass (NFData, ToJSON, FromJSON)

makeLenses ''CovLoc

instance Pretty CovLoc where
  pretty (CovLoc file l1 l2 c1 c2) =
    mconcat [ pretty file, ":", pretty l1, ",", pretty c1, "-",  pretty l2, ",", pretty c2]

data CoverageAnnotation = CoverLocation CovLoc
                        | CoverBool CovLoc Bool
                        deriving (Ord, Eq, Show, Read, Generic, Serialise)
                        deriving Flat via (AsSerialize CoverageAnnotation)
                        deriving anyclass (NFData, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance Pretty CoverageAnnotation where
  pretty (CoverLocation loc) = pretty loc
  pretty (CoverBool loc b)   = pretty loc <+> "=" <+> pretty b

data Metadata = ApplicationHeadSymbol String
              | IgnoredAnnotation   -- ^ Location that is not interesting to cover.
    deriving (Ord, Eq, Show, Generic, Serialise)
    deriving Flat via (AsSerialize Metadata)
    deriving anyclass (NFData, ToJSON, FromJSON)

instance Pretty Metadata where
  pretty = viaShow

newtype CoverageMetadata = CoverageMetadata { _metadataSet :: Set Metadata }
    deriving (Ord, Eq, Show, Generic)
    deriving anyclass (Serialise, NFData, ToJSON, FromJSON)
    deriving newtype (Semigroup, Monoid)
    deriving Flat via (AsSerialize CoverageMetadata)

makeLenses ''CoverageMetadata

instance Pretty CoverageMetadata where
  pretty (CoverageMetadata s) = vsep . map pretty . Set.toList $ s

-- | This type keeps track of all coverage annotations and where they have been inserted / what
-- annotations are expected to be found when executing a piece of code.
data CoverageIndex = CoverageIndex { _coverageMetadata :: Map CoverageAnnotation CoverageMetadata }
                      deriving (Ord, Eq, Show, Generic, Serialise)
                      deriving Flat via (AsSerialize CoverageIndex)
                      deriving anyclass (NFData, ToJSON, FromJSON)

makeLenses ''CoverageIndex

coverageAnnotations :: Getter CoverageIndex (Set CoverageAnnotation)
coverageAnnotations = coverageMetadata . to Map.keysSet

ignoredAnnotations :: Getter CoverageIndex (Set CoverageAnnotation)
ignoredAnnotations = coverageMetadata
                   . to (Map.keysSet . Map.filter (Set.member IgnoredAnnotation . _metadataSet))

instance Semigroup CoverageIndex where
  ci <> ci' = CoverageIndex (Map.unionWith (<>) (_coverageMetadata ci) (_coverageMetadata ci'))

instance Monoid CoverageIndex where
  mempty = CoverageIndex Map.empty

-- | Include a location coverage annotation in the index
addLocationToCoverageIndex :: MonadWriter CoverageIndex m => CovLoc -> m CoverageAnnotation
addLocationToCoverageIndex src = do
  let ann = CoverLocation src
  tell $ CoverageIndex $ Map.singleton ann mempty
  pure ann

-- | Include a boolean coverage annotation in the index
addBoolCaseToCoverageIndex :: MonadWriter CoverageIndex m => CovLoc -> Bool -> CoverageMetadata -> m CoverageAnnotation
addBoolCaseToCoverageIndex src b meta = do
  let ann = boolCaseCoverageAnn src b
  tell $ CoverageIndex (Map.singleton ann meta)
  pure ann

{-# INLINE boolCaseCoverageAnn #-}
boolCaseCoverageAnn :: CovLoc -> Bool -> CoverageAnnotation
boolCaseCoverageAnn src b = CoverBool src b

newtype CoverageData = CoverageData { _coveredAnnotations :: Set CoverageAnnotation }
  deriving (Ord, Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NFData, ToJSON, FromJSON)

makeLenses ''CoverageData

data CoverageReport = CoverageReport { _coverageIndex :: CoverageIndex
                                     , _coverageData  :: CoverageData }
  deriving (Ord, Eq, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

makeLenses ''CoverageReport

instance Semigroup CoverageReport where
  CoverageReport i1 d1 <> CoverageReport i2 d2 = CoverageReport (i1 <> i2) (d1 <> d2)

instance Monoid CoverageReport where
  mempty  = CoverageReport mempty mempty
  mappend = (<>)

coverageDataFromLogMsg :: String -> CoverageData
coverageDataFromLogMsg = foldMap (CoverageData . Set.singleton) . readMaybe

pprCoverageReport :: CoverageReport -> Doc ann
pprCoverageReport report =
  vsep $ ["=========[COVERED]=========="] ++
         [ nest 4 $ vsep (pretty ann : (map pretty . Set.toList . foldMap _metadataSet $ metadata ann))
         | ann <- Set.toList $ allAnns `Set.intersection` coveredAnns ] ++
         ["========[UNCOVERED]========="] ++
         (map pretty . Set.toList $ uncoveredAnns) ++
         ["=========[IGNORED]=========="] ++
         (map pretty . Set.toList $ ignoredAnns Set.\\ coveredAnns)
  where
    allAnns       = report ^. coverageIndex . coverageAnnotations
    coveredAnns   = report ^. coverageData  . coveredAnnotations
    ignoredAnns   = report ^. coverageIndex . ignoredAnnotations
    uncoveredAnns = allAnns Set.\\ (coveredAnns <> ignoredAnns)

    metadata ann = Map.lookup ann (report ^. coverageIndex . coverageMetadata)

