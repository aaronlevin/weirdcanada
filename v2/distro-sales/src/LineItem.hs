{-# LANGUAGE OverloadedStrings #-}

module LineItem where

import Control.Applicative ((<*>), (<$>))
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?), FromJSON(parseJSON), Value(Object))
import Data.Text
import GHC.Generics (Generic)
import Location (Location(Location))

-- | shopify represents prices as strings in json.
newtype Price = Price { unStringPrice :: Double } deriving (Eq, Ord, Show)

instance FromJSON Price where
  parseJSON (A.String p) = return . Price . read . unpack $ p
  parseJSON _            = mempty

data LineItem =
  LineItem { lineItemId :: Integer
           , lineItemVariantId :: Int
           , lineItemTitle :: Text
           , lineItemQuantity :: Int
           , lineItemPrice :: Price
           , lineItemSku :: Text
           , lineItemVariantTitle :: Maybe Text
           , lineItemProductId :: Int
           , lineItemOriginLocation :: Maybe Location
           , lineItemDestinationLocation :: Maybe Location
           } deriving (Eq, Ord, Show)

instance FromJSON LineItem where
  parseJSON (Object v) =
    LineItem <$> v .: "id"
             <*> v .: "variant_id"
             <*> v .: "title"
             <*> v .: "quantity"
             <*> v .: "price"
             <*> v .: "sku"
             <*> v .:? "variant_title"
             <*> v .: "product_id"
             <*> v .:? "origin_location"
             <*> v .:? "destination_location"
  parseJSON _ = mempty
