{-# LANGUAGE OverloadedStrings #-}

module Address where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson ((.:), (.:?), FromJSON(parseJSON), Value(Object))
import Data.Text
import GHC.Generics (Generic)

data Address =
  Address { firstName :: Maybe Text
          , lastName :: Maybe Text
          , address1 :: Maybe Text
          , address2 :: Maybe Text
          , city :: Text
          , zip :: Text
          , province :: Text
          , country :: Text
          }

instance FromJSON Address where
  parseJSON (Object v) =
    Address <$> v .:? "first_name"
            <*> v .:? "last_name"
            <*> v .: "address1"
            <*> v .: "address2"
            <*> v .: "city"
            <*> v .: "zip"
            <*> v .: "province"
            <*> v .: "country"
  parseJSON _ = mempty
