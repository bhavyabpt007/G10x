{-# LANGUAGE DeriveGeneric #-}
module Types where

import           GHC.Generics
import qualified Data.Aeson as A

data ParseJSONBody = ParseJSONBody { encodedVal :: String } 
    deriving (Generic,Show)
instance A.FromJSON ParseJSONBody
instance A.ToJSON ParseJSONBody
