{-# LANGUAGE DataKinds
   , TypeOperators
   , OverloadedStrings
   , FlexibleInstances
   , ScopedTypeVariables
   , KindSignatures
   , FlexibleContexts
   , RankNTypes
   , UndecidableInstances
   , GADTs
   #-}
{-| This adds and instance of FromJSON to SimpleRecords
 -
 -}
module Data.Vinyl.JSON where

import Control.Applicative
import Control.Monad

import Data.Vinyl
import Data.Vinyl.Idiom.Identity
-- import Data.ByteString as L
import Data.Aeson
import Data.Aeson.Types

import qualified Data.Text as T

import GHC.TypeLits
-- import Data.Proxy

instance FromJSON (PlainRec '[]) where
    parseJSON (Object v) = pure RNil
    parseJSON _ = mzero

instance (KnownSymbol sym, FromJSON a, FromJSON (PlainRec fields)) =>
        FromJSON (PlainRec ((sym ::: a) ': (fields :: [*]))) where
    parseJSON (Object v) = ((<+>) :: PlainRec '[sym ::: a] 
                                  -> PlainRec fields 
                                  -> PlainRec  ((sym ::: a) ': fields))
                                <$> ((field =:) <$> (v .: json_name))
                                <*> rest_rec
        where field = Field :: (sym ::: a)
              json_name = T.pack $ show field
              rest_rec = parseJSON (Object v)
              -- I had to type (<+>) because I can't say the type of rest_rec,
              -- as it uses an internal type
              -- rest_rec :: Data.Aeson.Types.Internal.Parser (PlainRec (fields))
              -- but I needed to type rest_rec's result record to have it parse

class HasPairs a where
  toPairs :: a -> [Pair]

instance HasPairs (PlainRec '[]) where
  toPairs _ = []

instance forall t rs s. (ToJSON t, HasPairs (PlainRec rs), KnownSymbol s)
  => HasPairs (PlainRec (s:::t ': rs)) where
  toPairs (Identity x :& xs) = 
    (T.pack (show (Field :: s:::t)), toJSON x) : toPairs xs

instance HasPairs (PlainRec rs) => ToJSON (PlainRec rs) where
  toJSON = object . toPairs


