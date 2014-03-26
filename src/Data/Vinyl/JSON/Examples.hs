{-# LANGUAGE DataKinds
   , TypeOperators
   , OverloadedStrings
   , FlexibleInstances
   , ScopedTypeVariables
   , KindSignatures
   , FlexibleContexts
   #-}

module Data.Vinyl.JSON.Examples where

import Control.Applicative
import Control.Monad

import Data.Vinyl

-- import Data.ByteString as L
import Data.Aeson

import Data.Text as T

import GHC.TypeLits

import Data.Vinyl.JSON

-- Examples of use
--{-
example_json = "{\"name\": \"jon\", \"job\":\"Code\", \"age\":42, \"things\":[1,2,3] }"

nested_json = "{\"dad\" : { \"name\" : \"bob\" } }"


name = Field :: ("name" ::: Text)

parsed_example :: Maybe (PlainRec '[("name" ::: Text),
                                    ("job"  ::: Text),
                                    ("age"  ::: Int),
                                    ("things" ::: [Int])])
parsed_example = decode example_json

parsed_nested :: Maybe (PlainRec '[("dad" ::: 
                            (PlainRec '[("name" ::: Text)]))])
parsed_nested = decode nested_json
--}


talk_about = case parsed_example of
                    Nothing -> putStrLn "Failed to parse"
                    (Just person) -> 
                        putStrLn ("He is named: " 
                                  ++ (T.unpack (rGet name person)))

