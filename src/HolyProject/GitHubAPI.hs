{-# LANGUAGE OverloadedStrings #-}
module HolyProject.GitHubAPI
    (searchGHUserFromEmail)
where

import qualified Data.ByteString.Lazy.Char8 as  LZ
-- HTTP request and JSON handling
import Network.HTTP.Conduit
import Control.Lens.Operators       ((^?))
import Control.Lens.Aeson
import Data.Aeson.Encode            (fromValue)
import qualified Data.Text.Lazy as TLZ
import qualified Data.Text.Lazy.Builder as TLB

-- | make a simple http request but add a user agent to the HTTP header
simpleHTTPWithUserAgent :: String -> IO LZ.ByteString
simpleHTTPWithUserAgent url = do
    r  <- parseUrl url
    let request = r { requestHeaders =  [ ("User-Agent","HTTP-Conduit") ] }
    body <- withManager $ \manager -> do
                response <- httpLbs request manager
                return $ responseBody response
    return body

-- | Ask the github API
-- A strange behaviour you HAVE TO add a User-Agent in your header.
-- It took me way too long to get this error
searchGHUserFromEmail :: String -> IO (Maybe String)
searchGHUserFromEmail ""    = return Nothing
searchGHUserFromEmail email = do
    let url = "https://api.github.com/search/users?q=" ++ email
    body <- simpleHTTPWithUserAgent url
    login <- return $ body ^? key "items" . nth 0 . key "login"
    return $ fmap jsonValueToString login
    where
        jsonValueToString = TLZ.unpack . TLB.toLazyText . fromValue
