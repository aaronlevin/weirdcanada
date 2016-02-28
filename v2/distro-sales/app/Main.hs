{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (mapM_)
import Lib (process)
import Data.Aeson ((.:), FromJSON, parseJSON, Value(Object), eitherDecode)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple (ConnectInfo (ConnectInfo),connect)
import System.Environment (getEnv, lookupEnv)
import Network.Wreq (asJSON, get, Response, responseBody)
import Order (Order(..))
import LineItem (unStringPrice)

data OrderResponse =
  OrderResponse { getOrders :: [Order] } deriving (Eq, Ord, Show)

instance FromJSON OrderResponse where
  parseJSON (Object v) = OrderResponse <$> v .: "orders"

main :: IO ()
main = do
  -- app configuration
  dbName          <- getEnv "PGDATABASE"
  dbHost          <- getEnv "PGHOST"
  dbUser          <- getEnv "PGUSER"
  dbPass          <- getEnv "PGPASSWORD"
  dbPort          <- maybe 5432 read <$> lookupEnv "PGPORT"
  limit           <- fromMaybe "50" <$> lookupEnv "LIMIT"
  let connectInfo = ConnectInfo dbHost dbPort dbUser dbPass "weirdcanada"
  conn <- connect connectInfo
  let url = "https://94b0f5cfbb6797cf72657e68870afbec:22a792f7bf22ff4d2b489fc729bb5270@weirdcanada.myshopify.com/admin/orders.json?limit=" ++ limit
  r <- fmap (getOrders . flip (^.) responseBody) (get url >>= asJSON)
  process conn r
