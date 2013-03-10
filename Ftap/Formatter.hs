{-# LANGUAGE OverloadedStrings #-}
module Ftap.Formatter where

import Control.Applicative
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Maybe as Mb
import qualified Data.ByteString.Char8 as BS

data Status = Status {
  statusText :: String
  } deriving Show

instance FromJSON Status where
  parseJSON (Object v) = Status <$> v .: "text"

formatStatuses :: Value -> [String]
formatStatuses v = let r = fromJSON v :: Result (V.Vector Status) in case r of
  Success statuses -> V.toList $ V.map statusText statuses
