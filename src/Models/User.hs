{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

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
  -- loc :: Field User Location,
  primary_day :: Field User Int
} deriving (Show, Generic)

instance Schema User where
  schema = User {
    _id = field "_id",
    first_name = field "first_name",
    last_name = field "last_name",
    updated_at = field "updated_at",
    roles = field "roles",
    -- loc = field "location",
    primary_day = field "primary_day"
  }

instance Queryable User where
  collection _ = "users"

data Location = Location {
  city :: Field Location String,
  postal_code :: Field Location String
} deriving (Show, Eq)

instance Schema Location where
  schema = Location {
    city = field "city",
    postal_code = field "postal_code"
  }

run :: (Schema r, Generic r, GParse (Rep r), Queryable m) => Query m r -> IO [r]
run q = do
  db <- getDB "v2-staging"
  q $. fetch db

q1 = find [ first_name $= "Jason" ] $. limit 10
q2 = find [ first_name $= "Jason", roles $*= "customer" ] $. select _id $. limit 5 $. asc last_name
-- q3 = find [ loc /. city $= "New York" ] $. select (loc /. postal_code)
q4 = find [ first_name $? True ]
q5 = find [ primary_day $>= 1, primary_day $<= 5 ]

