{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Models.User where

import GHC.Generics
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Models.Query
import qualified Database.MongoDB as Mongo
import Data.Bson (ObjectId, (=:), Value(Doc))

data User = User {
  _id :: Field User ObjectId,
  first_name :: Field User String,
  last_name :: Field User String,
  updated_at :: Field User UTCTime,
  roles :: Field User [String],
  loc :: Field User Location
}

data Location = Location {
  city :: Field Location String,
  postal_code :: Field Location String
} deriving (Show, Eq)

instance Embeddable Location where
  schema2 = Location {
    city = field2 "city",
    postal_code = field2 "postal_code"
  }

instance Mongo.Val Location where
  val a = Doc [ ]
  cast' doc = undefined

instance Queryable User where
  collection _ = "users"
  schema = User {
    _id = field "_id",
    first_name = field "first_name",
    last_name = field "last_name",
    updated_at = field "updated_at",
    roles = field "roles",
    loc = field "location"
  }

run :: Queryable m => Query m r -> IO [Mongo.Document]
run q = do
  db <- getDB "v2-staging"
  q $. fetch db

q1 = find [ first_name $= "Jason" ] $. limit 10
q2 = find [ first_name $= "Jason", roles $*= "customer" ] $. select _id
q3 = find [ loc /. city $= "New York" ] $. select (loc /. postal_code)

