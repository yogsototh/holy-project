{-# LANGUAGE OverloadedStrings #-}
module HolyProject.GithubAPI
    (searchGHUser)
where

import qualified Data.ByteString.Lazy.Char8 as  LZ
-- HTTP request and JSON handling
import Network.HTTP.Conduit
import Control.Lens.Operators       ((^?))
import Data.Aeson.Encode            (encodeToTextBuilder)
import Data.Aeson.Lens              (key,nth)
import qualified Data.Text.Lazy as TLZ
import qualified Data.Text.Lazy.Builder as TLB
import Control.Monad                ((<=<))

-- | make a simple http request but add a user agent to the HTTP header
-- You HAVE TO add a User-Agent in your header to use the github API.
simpleHTTPWithUserAgent :: String -> IO LZ.ByteString
simpleHTTPWithUserAgent url = do
    r  <- parseUrl url
    let request = r { requestHeaders =  [ ("User-Agent","HTTP-Conduit") ] }
    withManager $ (return.responseBody) <=< httpLbs request

-- | Search a username using the github API
searchGHUser :: String -> IO (Maybe String)
searchGHUser ""    = return Nothing
searchGHUser email = do
    let url = "https://api.github.com/search/users?q=" ++ email
    body <- simpleHTTPWithUserAgent url
    let login = body ^? key "items" . nth 0 . key "login"
    return $ fmap jsonValueToString login
    where
        jsonValueToString = TLZ.unpack . TLB.toLazyText . encodeToTextBuilder
