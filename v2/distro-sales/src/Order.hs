{-# LANGUAGE OverloadedStrings #-}

module Order where

import Data.Aeson ((.!=), (.:), (.:?), FromJSON(parseJSON), Value(Object))
import qualified Data.Aeson as A
import Data.Text (unpack, Text)
import Data.Time (UTCTime)
import Customer(Customer)
import LineItem(Price, LineItem(LineItem))
import Location(Location)

data Order =
  Order { orderId :: Integer
        , orderEmail :: Maybe Text -- empty string = Nothing
        , orderTotalPrice :: Maybe Price
        , orderSubtotalPrice :: Maybe Price
        , orderProcessedAt :: Maybe Text
        , orderLineItems :: [LineItem]
        , orderCreatedAt :: UTCTime
        , orderShippingAddress :: Maybe Location
        , orderBillingAddress :: Maybe Location
        , orderCustomer :: Maybe Customer
        } deriving (Eq, Ord, Show)

instance FromJSON Order where
  parseJSON (Object v) =
    Order <$> v .: "id"
          <*> v .:? "email"
          <*> v .:? "total_price"
          <*> v .:? "subtotal_price"
          <*> v .:? "processed_at"
          <*> v .: "line_items"
          <*> v .: "created_at"
          <*> v .:? "shipping_address"
          <*> v .:? "billing_address"
          <*> v .:? "customer"
