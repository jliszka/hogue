{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Data.Monoid
import Control.Monad.IO.Class
import Web.Spock.Safe
import Network.Wai.Middleware.RequestLogger
import GHC.Generics
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Models.User as User
import Models.Request as Request
import Models.Query
import Network.HTTP.Types.Status

(//) = (<//>)


main :: IO ()
main =
  runSpock 8080 $ spockT id $ do
    db <- liftIO $ getDB "v2-staging"
    middleware logStdoutDev

    get root $
      text "Hello!"

    subcomponent "customers" $ customersRoutes db


-- USERS ROUTES

customersRoutes db = do
  get root $ do
    customers <- liftIO $ find [ User.roles `contains` "customer" ] $. limit 10 $. fetch db
    json customers

  get var $ requireCustomer db $ \customer -> do
    json customer

  get (var // "requests") $ requireCustomer db $ \customer -> do
    let cid = customer ~. User._id
    requests <- liftIO $ find [ Request.customer_id `eqs` cid ] $.fetch db
    json requests


-- HELPERS

requireCustomer :: DB -> (User -> ActionT IO ()) -> String -> ActionT IO ()
requireCustomer db action customerId = do
  let oid = read customerId
  maybeCustomer <- liftIO $ find [ User._id `eqs` oid, User.roles `contains` "customer" ] $. fetchOne db
  case maybeCustomer of
    Nothing -> setStatus status404
    Just customer -> action customer
