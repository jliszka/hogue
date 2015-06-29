{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Models.Request where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import GHC.Generics
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Models.Query
import qualified Models.User as User
import Data.Bson (ObjectId)

type RequestId = NewId Request

data Request = Request {
  _id         :: Field Request RequestId,
  customer_id :: Field Request User.UserId,
  desc        :: Field Request String
} deriving (Show, Generic, ToJSON)

instance Schema Request where
  schema = Request {
    _id         = field "_id",
    customer_id = field "customer_id",
    desc        = field "desc"
  }

instance Queryable Request where
  collection _ = "requests"
  primaryKey = _id


-- q :: Query Request Request
-- q = find [ _id $> 3, user_id $= "hi" ] $. orderAsc _id $. limit 10

go :: MaybeT IO [Request]
go = do
  db <- lift $ getDB "v2-staging"
  let oid = read "5543eae3cfaa950f00dffbd6"
  uid <- MaybeT $ find [ User._id `eqs` oid ] $. select User._id $. fetchOne db
  lift $ find [ customer_id `eqs` uid ] $. fetch db
