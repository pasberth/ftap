{-# LANGUAGE OverloadedStrings #-}
module Ftap.Formatter where

import Control.Applicative
import Data.Aeson
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Maybe as Mb
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

data Status = Status {
  statusText :: String
, statusUser :: User
  }

data User = User {
  userId :: Int
, userScreenName :: String
  }

instance FromJSON Status where
  parseJSON (Object v) = Status <$> v .: "text" <*> v .: "user"

instance FromJSON User where
  parseJSON (Object v) = User <$> v .: "id" <*> v .: "screen_name"

formatStatus ::  Status -> [Status -> String] ->String
formatStatus s = L.intercalate "\t" . map ($s)

formatStatuses :: Value -> [Status -> String] -> [String]
formatStatuses v fs = let r = fromJSON v :: Result (V.Vector Status) in case r of
  Success statuses -> map (flip formatStatus fs) (V.toList statuses)
  Error s -> [s]
