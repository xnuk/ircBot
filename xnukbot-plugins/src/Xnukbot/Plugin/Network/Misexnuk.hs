{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TupleSections #-}
module Xnukbot.Plugin.Network.Misexnuk where

import "http-conduit" Network.HTTP.Conduit (http, parseRequest_, responseBody, setQueryString, newManager, tlsManagerSettings)
import Xnukbot.Plugin.Network.Misexnuk.Daummap (Dildo(Dildo, lat, lng), search)
import Xnukbot.Plugin.Network.Misexnuk.Html (crawl)

import Data.Function ((&))
import "conduit-extra" Data.Conduit.Text (decode, utf8)
import "resourcet" Control.Monad.Trans.Resource (runResourceT)
import "conduit" Data.Conduit (($$+-), (=$=))
import "text" Data.Text.Encoding (encodeUtf8)
import "text" Data.Text (Text, stripPrefix, strip)
import qualified "text" Data.Text as T (null)

import "xnukbot" Xnukbot.Plugin.Simple (privPlugin', noprefChecker, Msg(setting))
import "xnukbot" Xnukbot.Plugin (Plugin)

import "bytestring" Data.ByteString (ByteString)

get :: ByteString -> Text -> IO [Text]
get key place = do
    a <- search key (encodeUtf8 place)
    case a of
        Right Dildo{lat, lng} -> do
            let req = parseRequest_ "http://m.airkorea.or.kr/sub_new/sub11.jsp" &
                    setQueryString [("lat", Just $ encodeUtf8 lat), ("lng", Just $ encodeUtf8 lng)]
            manager <- newManager tlsManagerSettings
            runResourceT $ do
                resp <- http req manager
                responseBody resp $$+- (decode utf8 =$= crawl)
        Left x -> print (show x) >> return ["\129300"] -- 🤔

plugin :: ByteString -> Plugin
plugin key = privPlugin' "Misexnuk" $ \msg -> do
    place <- noprefChecker (const $ fmap strip . stripPrefix "미세즈눅 ") msg
    return (setting msg, if T.null place then return [] else fmap (True,) <$> get key place)
