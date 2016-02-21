{-# LANGUAGE OverloadedStrings #-}

module Customer where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson ((.:), (.:?), FromJSON(parseJSON), Value(Object))
import Data.Time (UTCTime)
import Data.Text (Text)
import Location (Location)

data Customer =
  Customer { customerId :: Integer
           , customerCreatedAt :: UTCTime
           , customerEmail :: Maybe Text
           , customerFirstname :: Maybe Text
           , customerLastName :: Maybe Text
           , customerAddress :: Maybe Location
           } deriving (Eq, Ord, Show)

instance FromJSON Customer where
  parseJSON (Object v) =
    Customer <$> v .: "id"
             <*> v .: "created_at"
             <*> v .:? "email"
             <*> v .:? "first_name"
             <*> v .:? "last_name"
             <*> v .:? "default_address"
