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

(//) = (<//>)


main :: IO ()
main =
  runSpock 8080 $ spockT id $ do
  middleware logStdoutDev

  get root $
    text "Hello!"

  subcomponent "users" $ usersRoutes
  subcomponent "requests" $ requestsRoutes


usersRoutes = do
  get (var // "requests") $ \userId -> do
    now <- liftIO getCurrentTime
    -- let u = User { _id = userId, name = "Alfred", created_at = now }
    -- let q = find [ Request._id ~> (Gt 3), Request.user_id ~> (Eq "hi") ] -- { limit = Just 10 }
    -- let r = Request { _id = "1", user_id = User._id u, created_at = now }
    -- let r2 = r { Request._id = requestId }
    text $ userId

requestsRoutes = do
  get (var) $ \name ->
    text ("Hello " <> name <> "!")
