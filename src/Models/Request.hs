{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Models.Request where

import GHC.Generics
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Models.Query

data Request = Request {
  _id :: Field Request Int,
  user_id :: Field Request String
} deriving Generic

instance Schema Request where
  schema = Request {
    _id = field "_id",
    user_id = field "user_id"
  }

instance Queryable Request where
  collection _ = "requests"


q :: Query Request Request
q = find [ _id $> 3, user_id $= "hi" ] $. asc _id $. limit 10

r :: Request
r = Request {
  _id = val 1,
  user_id = val "hi"
}

i :: Int
i = r ~. _id

{-
data Request = Request {
  _id :: String,
  user_id :: String,
  created_at :: UTCTime
} deriving (Show, Eq, Generic, ToJSON)
-}

