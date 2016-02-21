{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Sale where

import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe (fromMaybe)
import           Database.PostgreSQL.Simple.Types (Default(Default))
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import           Data.Time (UTCTime)
import           Data.Text

-- | sale as represented in the database
data DBSale =
  DBSale { saleFormat          :: Int
         , saleCountry         :: Maybe Text
         , saleId              :: Maybe Integer
         , saleAmount          :: Double
         , saleDatetime        :: UTCTime -- ???
         , saleCity            :: Maybe Text
         , saleProvince        :: Maybe Text
         , salePostalCode      :: Maybe Text
         , saleConsignor       :: Integer
         , saleAddressLine1    :: Maybe Text
         , saleAddressLine2    :: Maybe Text
         , saleSku             :: Text
         , saleConsignedItemId :: Integer
         , saleOrderId         :: Integer
         , saleLineItemId      :: Integer
         , saleQuantity        :: Int
         , saleMarkup          :: Double
         , salePaidToConsignor :: Maybe Double
         , saleCustomerId      :: Maybe Integer
         , saleWholesale       :: Maybe Bool
         , saleRefund          :: Maybe Double
         , saleRefundNote      :: Maybe Double
         } deriving (Eq, Ord, Show)

instance ToRow DBSale where
  toRow DBSale{..} = [ toField saleFormat
                     , toField saleCountry
                     , maybe (toField Default) toField saleId
                     , toField saleAmount
                     , toField saleDatetime
                     , toField saleCity
                     , toField saleProvince
                     , toField salePostalCode
                     , toField saleConsignor
                     , toField saleAddressLine1
                     , toField saleAddressLine2
                     , toField saleSku
                     , toField saleConsignedItemId
                     , toField saleOrderId
                     , toField saleLineItemId
                     , toField saleQuantity
                     , toField saleMarkup
                     , toField salePaidToConsignor
                     , toField saleCustomerId
                     , toField saleWholesale
                     , toField saleRefund
                     , toField saleRefundNote
                     ]
