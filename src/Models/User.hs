{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Models.User where

import GHC.Generics
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)

data User = User {
  _id :: String,
  name :: String,
  created_at :: UTCTime
} deriving (Show, Eq, Generic, ToJSON)
