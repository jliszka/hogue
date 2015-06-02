{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}

module Models.User where

import GHC.Generics
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Models.Query
import qualified Database.MongoDB as Mongo
import Data.Bson (ObjectId, (=:), Value(Doc), Document)

data User = User {
  _id         :: Field User ObjectId,
  first_name  :: Field User String,
  last_name   :: Field User String,
  updated_at  :: Field User UTCTime,
  roles       :: Field User [String],
  loc         :: OptField User Location,
  primary_day :: OptField User Int
} deriving (Show, Generic, ToJSON)

instance Schema User where
  schema = User {
    _id         = field "_id",
    first_name  = field "first_name",
    last_name   = field "last_name",
    updated_at  = field "updated_at",
    roles       = field "roles",
    loc         = efield "location",
    primary_day = field "primary_day"
  }

instance Queryable User where
  collection _ = "users"

data Location = Location {
  city        :: Field Location String,
  postal_code :: Field Location String
} deriving (Show, Generic, ToJSON)

instance Schema Location where
  schema = Location {
    city        = field "city",
    postal_code = field "postal_code"
  }

run :: a -> (DB -> a -> IO b) -> IO b
run q f = do
  db <- getDB "v2-staging"
  f db q

q1 = find [ first_name $= "Jason" ] $. limit 10
q2 = find [ first_name $= "Jason", roles $*= "customer" ] $. select last_name $. limit 5 $. asc last_name
q2' = find [ first_name $= "Jason", roles $*= "customer" ] $. select _id $. limit 5 $. asc last_name
q3 = find [ loc /. city $= "New York" ] $. select (loc /. postal_code)
q3' = find [ loc /. city $= "New York" ] $. select primary_day
q4 = find [ first_name $? True ]
q5 = find [ primary_day $>= 1, primary_day $<= 5 ]
q6 = find [ last_name $= "Test" ] -- $. select (loc /. postal_code)

m1 = find [ last_name $= "Test" ] $. modify [ last_name $:= "Liszka" ]