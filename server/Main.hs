{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Char8 (unpack)
import Network.Wai (requestBody, requestMethod, responseLBS,
                    Application, Request(..), Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types (status200, HttpVersion(..))
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson (decode, Value, ToJSON(..), object, (.=))
import qualified Data.Aeson as Aeson (Value(..))
import Data.Aeson.Encode.Pretty (encodePretty, Indent(..))
import Data.Maybe (fromJust)
import qualified Data.Text as Text (pack)

import Lib

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Server listening on port " ++ show port
  run port (logAllMiddleware $ logStdout app)

app :: Application
app req f =
  f $ responseLBS status200 [(hContentType, "text/plain")] (encodePretty $ req)

-- "Application is just a handler: "
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

-- Pretty prints the entire HTTP request.
logAllMiddleware :: Middleware
logAllMiddleware app req respond = do
  -- It's not even JSON bro
  print $ show req
  -- print $ encodePretty $ fromJust (decode $ pack $ show req :: Maybe Value)
  app req respond

-- TODO: finish this ...
instance ToJSON Request where
  toJSON req =
    object [
      "requestMethod" .= (unpack $ requestMethod req),
      "httpVersion" .= (toJSON $ httpVersion req),
      "rawPathInfo" .= (unpack $ rawPathInfo req),
      "rawQueryString" .= (unpack $ rawQueryString req)
    ]

instance ToJSON HttpVersion where
  toJSON = Aeson.String . Text.pack . show
