{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module DB where

import Data.Int (Int64)
import Data.Map.Strict (Map, fromList)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, executeMany, Only (Only), In(In), Query, query)
import Sale (DBSale)

-- markup and consigned item

dbConsignedItemBySkusQ :: Query
dbConsignedItemBySkusQ = "select sku, id, markup, consignor from consigneditem where sku in ?"

dbConsignedItemBySkus :: Connection
                      -> [Text]      -- skus
                      -> IO (Map Text (Integer, Maybe Double, Maybe Integer))
dbConsignedItemBySkus conn skus =
  fromList . fmap (\(!w,!x,!y,!z) -> (w,(x, fromRational <$> y, z))) <$> query conn dbConsignedItemBySkusQ (Only $ In skus)

dbSalesAlreadyInsertedQ :: Query
dbSalesAlreadyInsertedQ =
  "select orderid from sale where orderid in ?"

dbSalesAlreadyInserted :: Connection
                       -> [Integer]
                       -> IO [Integer]
dbSalesAlreadyInserted conn orderids =
  let fromOnly (Only !i) = i
  in fmap fromOnly <$> query conn dbSalesAlreadyInsertedQ (Only $ In orderids)

dbCheckSaleExistsQ :: Query
dbCheckSaleExistsQ = "select count(*) from sale where orderid = ? and sku = ?"

dbCheckSaleExists :: Connection
                  -> Integer
                  -> Text
                  -> IO Bool
dbCheckSaleExists conn orderid sku = do
  [Only !i] <- query conn dbCheckSaleExistsQ (orderid, sku) :: IO [Only Int]
  return (i > 0)

dbInsertSalesQ :: Query
dbInsertSalesQ = "insert into sale values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

--  format          | bigint                      | 
--  country         | character varying(32)       | 
--  id              | bigint                      | not null default nextval('sale_id_seq'::regclass)
--  amount          | numeric(7,2)                | 
--  datetime        | timestamp without time zone | 
--  city            | character varying(32)       | 
--  province        | character varying(32)       | 
--  postalcode      | character varying(16)       | 
--  consignor       | bigint                      | 
--  addressline1    | character varying(256)      | 
--  addressline2    | character varying(256)      | 
--  sku             | character varying(32)       | 
--  consigneditem   | bigint                      | 
--  orderid         | bigint                      | 
--  lineitemid      | bigint                      | 
--  quantity        | integer                     | 
--  markup          | numeric(7,2)                | 
--  paidtoconsignor | numeric(7,2)                | 
--  customerid      | bigint                      | 
--  wholesale       | boolean                     | 
--  refund          | numeric(7,2)                | 
--  refundnote      | text                        | 

dbInsertSales :: Connection
              -> [DBSale]
              -> IO Int64
dbInsertSales conn = executeMany conn dbInsertSalesQ
