{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Lib where

import           Control.Arrow                     ((***))
import           Control.Monad                     (foldM, forM_)
import           Control.Monad.Identity            (Identity, runIdentity)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
import           Control.Monad.Writer.Class        (MonadWriter, tell)
import           Customer                          (Customer (..))
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.ByteString.Lazy              (ByteString)
import           Data.CountryCodes                 (toName)
import           Data.Map.Strict                   (Map, lookup, map)
import           Data.Maybe                        (catMaybes)
import           Data.Set                          (Set, fromList, member, (\\))
import           Data.Text                         (Text)
import           Data.Time                         (UTCTime)
import           Database.PostgreSQL.Simple        (Connection)
import           DB                                (dbConsignedItemBySkus,
                                                    dbInsertSales,
                                                    dbSalesAlreadyInserted)
import           GHC.Base                          ((<|>))
import           LineItem                          (LineItem (..), Price (..))
import           Location                          (Location (..),
                                                    defaultLocation)
import           Order                             (Order (..))
import           Prelude                           hiding (lookup, map)
import           Sale                              (DBSale (..))

newtype ConsignorId =
  ConsignorId { unConsignorId :: Integer } deriving (Eq, Ord, Show)

newtype ConsignedItemId =
  ConsignedItemId { unConsignedItemId :: Integer } deriving (Eq, Ord, Show)

newtype ConsignedItemMarkUp =
  ConsignedItemMarkUp { unConsignedItemMarkup :: Double } deriving (Eq, Ord, Show)

lineToSale :: Integer             -- order id
           -> UTCTime             -- order created at
           -> ConsignorId         -- consignor id
           -> ConsignedItemId     -- consigned Item Id
           -> ConsignedItemMarkUp -- consigneditem marup
           -> Maybe Location      -- shipping address
           -> Maybe Location      -- Billing address
           -> Maybe Integer       -- customer id
           -> LineItem            -- the line item
           -> DBSale
lineToSale orderId
           orderCreatedAt
           (ConsignorId consignorId)
           (ConsignedItemId consignedItemId)
           (ConsignedItemMarkUp markup)
           mShipAddr
           mBillingAddr
           mCustId
           LineItem{..}
           =
  let loc = mShipAddr <|> mBillingAddr <|> Just defaultLocation
  in DBSale { saleFormat = 0
            , saleCountry = fmap toName (lineItemDestinationLocation >>= locationCountryCode)
            , saleId = Nothing
            , saleAmount = unStringPrice lineItemPrice
            , saleDatetime = orderCreatedAt
            , saleCity = loc >>= locationCity
            , saleProvince = loc >>= locationProvinceCode
            , salePostalCode = loc >>= locationZip
            , saleConsignor = consignorId
            , saleAddressLine1 = loc >>= locationAddress1
            , saleAddressLine2 = loc >>= locationAddress2
            , saleSku = lineItemSku
            , saleConsignedItemId = consignedItemId
            , saleOrderId = orderId
            , saleLineItemId = lineItemId
            , saleQuantity = lineItemQuantity
            , saleMarkup = markup * fromInteger (toInteger lineItemQuantity)
            , salePaidToConsignor = Just 0.0
            , saleCustomerId = mCustId
            , saleWholesale = Nothing
            , saleRefund = Nothing
            , saleRefundNote = Nothing
            }

-- | each error carries the orderid
data SaleError = SkuDNE Integer (Maybe Text)
               | ConsignorDNE Integer
               | MarkUpDNE Integer
               deriving (Eq, Ord, Show)

getDBItems :: (MonadWriter [SaleError] m)
           => Map Text (ConsignedItemId, Maybe ConsignedItemMarkUp, Maybe ConsignorId)
           -> Order
           -> m [DBSale]
getDBItems skuMap Order {..} =
  let go oId oDate oShipLoc oBillLoc mCustId dbSales lineItem@LineItem{..} =
        case lineItemSku >>= flip lookup skuMap of
          Nothing -> tell [SkuDNE oId lineItemSku] >> return dbSales
          Just (_, Nothing, _) -> tell [ConsignorDNE oId ] >> return dbSales
          Just (_, _, Nothing) -> tell [MarkUpDNE oId] >> return dbSales
          Just (cItemId, Just cMarkUp, Just cId) ->
            let sale = lineToSale oId
                                  oDate
                                  cId
                                  cItemId
                                  cMarkUp
                                  oShipLoc
                                  oBillLoc
                                  mCustId
                                  lineItem
            in return (sale : dbSales)
      foldGo = go orderId
                  orderCreatedAt
                  orderShippingAddress
                  orderBillingAddress
                  (customerId <$> orderCustomer)
  in foldM foldGo [] orderLineItems

process :: Connection
        -> [Order]
        -> IO ()
process conn orders = do
  -- order ids
  let orderids = fmap orderId orders

  -- given the order ids, find the orders already present in the database.
  orderIdsPresentSet <- fromList <$> dbSalesAlreadyInserted conn orderids

  -- Create a set to efficiently find the unique, new orders to put in the db.
  let newOrderIdSet = fromList orderids \\ orderIdsPresentSet

  -- print new orders
  print ("The following Ids are unique: " ++ show newOrderIdSet)

  -- filter our original order list.
  let filteredOrders = filter (flip member newOrderIdSet . orderId) orders
      lineitems = filteredOrders >>= orderLineItems
      skus = fmap lineItemSku lineitems
      orderedLineItems = filteredOrders >>= \o -> fmap ((,) o) (orderLineItems o)

  let cFunc (mId, mMarkUp, cId) = (,,) (ConsignedItemId mId)
                                      (fmap ConsignedItemMarkUp mMarkUp)
                                      (fmap ConsignorId cId)
  -- consignedItemMap :: Map Text (Maybe ConsignedItemId, Maybe ConsignedItemMarkUp)
  consignedItemMap <- map cFunc <$> dbConsignedItemBySkus conn (catMaybes skus)

  -- loop through the filtered orders and construct items to be inserted into the db.
  let dbSalesAction =
        foldM (\dbs o -> (++ dbs) <$> getDBItems consignedItemMap o) [] filteredOrders
      (dbSales,errors) = (runIdentity . runWriterT) dbSalesAction

  -- print any encountered errors
  putStrLn "ERRORS"
  putStrLn "=========="
  forM_ errors print

  putStrLn ("items to be put in the database: " ++ show (length dbSales))
  forM_ dbSales print
  -- insert items into the database.

  effectedRows <- dbInsertSales conn dbSales

  putStrLn ("inserted: " ++ show effectedRows ++ "rows")
