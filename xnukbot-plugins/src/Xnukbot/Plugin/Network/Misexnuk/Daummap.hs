{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Network.Misexnuk.Daummap (Dildo(..), search) where

import "aeson" Data.Aeson ((.:), FromJSON(parseJSON), Value(Object, Null, Array), withObject)
import "aeson" Data.Aeson.Types (typeMismatch, Parser)
import "http-conduit" Network.HTTP.Conduit (parseRequest_, method, secure, requestHeaders, setQueryString, Request)
import "http-conduit" Network.HTTP.Simple (httpJSON, getResponseBody)
import Data.Function ((&))
import "text" Data.Text (Text, unpack)
import Control.Applicative ((<|>))
import "unordered-containers" Data.HashMap.Lazy (lookupDefault)
import "vector" Data.Vector ((!?))
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)

import "async" Control.Concurrent.Async (waitCatch, waitEitherCatch, withAsync)

import Control.Exception (SomeException)

race :: IO a -> IO a -> IO (Either SomeException a)
race a b =
    withAsync a $ \l ->
        withAsync b $ \r -> do
            z <- waitEitherCatch l r
            case z of
                Left (Left _) -> waitCatch r
                Left x -> return x
                Right (Left _) -> waitCatch l
                Right x -> return x

data Dildo = Dildo
    { title :: Text
    , lat :: Text
    , lng :: Text
    }

withKey :: String -> [Either Int Text]
        -> (String -> (b -> Parser a) -> Value -> Parser a)
        ->            (b -> Parser a)
        ->                               Value -> Parser a
withKey base (Right k:ks) z f (Object m) = withKey (base ++ '.':unpack k) ks z f (lookupDefault Null k m)
withKey base (Left  k:ks) z f (Array m) = withKey (base ++ '.':show k) ks z f (fromMaybe Null (m !? k))
withKey base [] z f x = z base f x
withKey base _ _ _ x = typeMismatch base x


instance FromJSON Dildo where
    parseJSON = withKey "Dildo" [Right "channel", Right "item", Left 0] withObject $ \o ->
        Dildo <$> o .: "title" <*> (o .: "latitude"  <|> o .: "lat") <*> (o .: "longitude" <|> o .: "lng")

{-# INLINE header #-}
header :: String -> Request
header s = (parseRequest_ s)
    { method = "GET"
    , secure = True
    , requestHeaders = [("Accept", "application/json")]
    }

searchKeyword, searchAddr :: FromJSON a => ByteString -> ByteString -> IO a
searchKeyword key place = fmap getResponseBody . httpJSON $ header "https://apis.daum.net/local/v1/search/keyword.json" & setQueryString
    [ ("apikey", Just key)
    , ("page", Just "1")
    , ("count", Just "1")
    , ("sort", Just "1")
    , ("query", Just place)
    ]

searchAddr key place = fmap getResponseBody . httpJSON $ header "https://apis.daum.net/local/geo/addr2coord" & setQueryString
        [ ("apikey", Just key)
        , ("pageno", Just "1")
        , ("output", Just "json")
        , ("page_size", Just "1")
        , ("q", Just place)
        ]

{-# INLINE search #-}
search :: FromJSON a => ByteString -> ByteString -> IO (Maybe a)
search key place =
    either (const Nothing) Just <$>
        race (searchKeyword key place) (searchAddr key place)

