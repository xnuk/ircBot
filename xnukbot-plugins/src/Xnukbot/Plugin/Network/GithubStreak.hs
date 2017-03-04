{-# LANGUAGE OverloadedStrings, ApplicativeDo, QuasiQuotes, NamedFieldPuns #-}

module Xnukbot.Plugin.Network.GithubStreak where

import "tagstream-conduit" Text.HTML.TagStream.Types (Token'(TagOpen))
import "tagstream-conduit" Text.HTML.TagStream.ByteString (Token, tokenStream)
import "conduit" Data.Conduit.List (mapMaybe, fold)
import "conduit" Data.Conduit ((=$=), ($$+-), Consumer)
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString.Char8 (unpack)
import "http-conduit" Network.HTTP.Conduit (parseRequest, checkResponse, responseStatus, newManager, tlsManagerSettings, http, responseBody)
import "http-types" Network.HTTP.Types.Status (statusIsSuccessful)
import "resourcet" Control.Monad.Trans.Resource (runResourceT)
import "safe" Safe (readMay, headMay)
--import "time" Data.Time.Calendar (Day, fromGregorian)
--import "attoparsec" Data.Attoparsec.ByteString.Char8 (decimal, char, parseOnly)
import "text" Data.Text (Text, pack)
import qualified "text" Data.Text as T
import Data.Monoid ((<>))
import "xnukbot" Xnukbot.Plugin.Simple (privPlugin', regexMatch, noprefChecker, Msg(Msg, nick, channel, setting))
import "xnukbot" Xnukbot.Plugin (Plugin, getAttr, setAttr, AttrT(Local))
import "pcre-heavy" Text.Regex.PCRE.Heavy (re)
{-
parseDate = either (const Nothing) Just . parseOnly parser
    where parser = fromGregorian <$> decimal <*> (char '-' *> decimal) <*> (char '-' *> decimal)
-}
getData :: Token -> Maybe Integer
getData (TagOpen "rect" attr True) = do
    cl <- lookup "class" attr

    if cl /= "day"
        then Nothing
        else do
            count <- lookup "data-count" attr >>= readMay . unpack
            --date  <- lookup "data-date"  attr >>= parseDate
            Just count

getData _ = Nothing

parse :: Monad m => Consumer ByteString m (Integer, Integer)
parse = tokenStream =$= mapMaybe getData =$= fold streak (0, 0)

streak :: (Integer, Integer) -> Integer -> (Integer, Integer)
streak _ 0 = (0, 0)
streak (count, days) x = (count+x, days+1)

run :: Text -> IO Text
run user = do
    req <- parseRequest ("https://github.com/users/" <> T.unpack user <> "/contributions")
    let request = req { checkResponse = \_ _ -> return () }
    manager <- newManager tlsManagerSettings
    runResourceT $ do
        res <- http request manager
        if statusIsSuccessful (responseStatus res)
            then do
                (count, days) <- responseBody res $$+- parse
                return $ (pack . show) days <> "일 연속 스트릭 (총 " <> (pack . show) count <> "커밋) https://github.com/" <> user
            else return "ㅇㅂㅇ)a"

plugin :: Plugin
plugin = privPlugin' "GithubStreak" $ \msg@Msg{channel, setting, nick} -> do
    let attr = "GithubStreak.nickname." <> nick
    user <- do
        x <- noprefChecker (const $ regexMatch [re|^스트릭\s*([A-Za-z0-9\-]*)\s*$|]) msg >>= headMay
        if T.null x
            then getAttr channel setting attr
            else Just x

    return
        ( setAttr (Local channel attr) user setting
        , do
            a <- run user
            return [(True, a)]
        )

