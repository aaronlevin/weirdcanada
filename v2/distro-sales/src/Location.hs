{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Location where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson ((.:), (.:?), FromJSON(parseJSON), Value(Object))
import Data.CountryCodes (CountryCode(CA))
import Data.Text
import GHC.Generics (Generic)

defaultLocation :: Location
defaultLocation =
  Location Nothing
           (Just CA)
           (Just "Alberta")
           Nothing
           (Just "DEFAULT")
           (Just "213 Omand Drive.")
           (Just "Edmonton")
           (Just "T6R 1L8")

data Location =
  Location { locationId :: Maybe Int
           , locationCountryCode :: Maybe CountryCode
           , locationProvinceCode :: Maybe Text
           , locationName :: Maybe Text
           , locationAddress1 :: Maybe Text
           , locationAddress2 :: Maybe Text
           , locationCity :: Maybe Text
           , locationZip :: Maybe Text
           } deriving (Eq, Ord)

instance Show Location where
  show Location{..} = show locationId
                   ++ "\t"
                   ++ show locationCountryCode
                   ++ "\t"
                   ++ show locationProvinceCode
                   ++ "\t"
                   ++ show locationName
                   ++ "\t"
                   ++ show locationAddress1
                   ++ "\t"
                   ++ show locationAddress2
                   ++ "\t"
                   ++ show locationCity
                   ++ "\t"
                   ++ show locationZip

instance FromJSON Location where
  parseJSON (Object v) =
    Location <$> v .:? "id"
             <*> v .:? "country_code"
             <*> v .:? "province_code"
             <*> v .:? "name"
             <*> v .:? "address1"
             <*> v .:? "address2"
             <*> v .:? "city"
             <*> v .:? "zip"
  parseJSON _ = mempty
