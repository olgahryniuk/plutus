{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
-- | A map represented as an "association list" of key-value pairs.
module PlutusTx.DataMap where

import Prelude qualified as Haskell

import PlutusTx.Builtins qualified as P hiding (head)
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.IsData as IsData
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (all, filter, mapMaybe, null, toList)
import PlutusTx.Prelude qualified as P
import PlutusTx.These

import Control.DeepSeq (NFData)
import Data.Data
import GHC.Generics (Generic)
import PlutusTx.AsData qualified as AsData
import Prettyprinter (Pretty (..))

{- HLINT ignore "Use newtype instead of data" -}

-- | A 'Map' of key-value pairs.


AsData.asData [d|
  data Ziyang = Liu Integer | He Integer Integer
     deriving newtype (P.Eq, FromData, UnsafeFromData, ToData)
  |]

AsData.asData [d|
  data Map k v = Map {unMap :: [(k, v)]}
     deriving newtype (P.Eq, FromData, UnsafeFromData, ToData)
  |]

{-# INLINEABLE fromList #-}
fromList :: (ToData k, ToData v, UnsafeFromData k, UnsafeFromData v) => [(k, v)] -> Map k v
fromList = Map

{-# INLINEABLE toList #-}
toList :: (ToData k, ToData v, UnsafeFromData k, UnsafeFromData v) => Map k v -> [(k, v)]
toList (Map l) = l

AsData.asData [d|
  data Foo = Bar { xxxxx :: Integer, yyyyy :: Integer, zzzzz :: BuiltinByteString,
    aaa :: Integer, bbb :: Integer }
    deriving newtype (P.Eq, IsData.FromData, IsData.UnsafeFromData, IsData.ToData)
  |]

newtype Snooker = Snooker P.BuiltinData
  deriving newtype (P.Eq, IsData.FromData, IsData.UnsafeFromData, IsData.ToData)

myx :: Snooker -> Integer
myx (Snooker d) =
  let xx = BI.unsafeDataAsConstr d
      ii = BI.fst xx
      ds = BI.snd xx
  in if ii P.== 0
       then BI.unsafeDataAsI (BI.head ds)
       else P.error ()

myy :: Snooker -> Integer
myy (Snooker d) =
  let xx = BI.unsafeDataAsConstr d
      ii = BI.fst xx
      ds = BI.snd xx
  in if ii P.== 0
       then BI.unsafeDataAsI (BI.head (BI.tail ds))
       else P.error ()

-- myx :: Foo -> Integer
-- myx (Bar d) = undefined
