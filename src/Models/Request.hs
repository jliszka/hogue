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
}

instance Queryable Request where
  collection _ = "requests"
  schema = Request {
    _id = field "_id",
    user_id = field "user_id"
  }

($$) :: a -> (a -> b) -> b
($$) a f = f a
infixl 9 $$

q :: Query Request
q = find [ _id ~> (Gt 3), user_id ~> (Eq "hi") ] $$ asc _id $$ limit 10

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

