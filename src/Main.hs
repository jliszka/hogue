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

    subcomponent "users" $ usersRoutes db


-- USERS ROUTES

usersRoutes db = do
  get root $ do
    users <- liftIO $ find [ User.roles $*= "customer" ] $. limit 10 $. fetch db
    json users

  get var $ requireUser db $ \user -> do
    json user


-- HELPERS

requireUser :: DB -> (User -> ActionT IO ()) -> String -> ActionT IO ()
requireUser db action userId = do
  let oid = read userId
  maybeUser <- liftIO $ find [ User._id $= oid ] $. fetchOne db
  case maybeUser of
    Nothing -> setStatus status404
    Just user -> action user
