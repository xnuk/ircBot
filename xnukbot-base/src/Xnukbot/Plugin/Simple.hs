{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Xnukbot.Plugin.Simple where

import Xnukbot.Plugin (MessageT(Message), PrefixT(..), Plugin, Plug, Setting, privmsgNoPref, privmsg, hasAttribute, removePrefix, Sender)
import "pcre-heavy" Text.Regex.PCRE.Heavy (Regex, scan)
import "text" Data.Text (Text, pack)
import "base" Control.Concurrent (forkIO)
import "base" Data.Monoid ((<>))
import Safe (headMay)
import Data.Maybe (mapMaybe)
import Control.Monad ((>=>))

data Msg = Msg
    { nick :: Text
    , channel :: Text
    , setting :: Setting
    , message :: Text
    }

privPlugin :: String -> (Sender -> Msg -> Maybe (IO Setting)) -> Plugin
privPlugin name func = (name, plug)
    where plug :: Plug
          plug setting' send (Message (Just NickName{nickName}) "PRIVMSG" [chan', message'])
            | hasAttribute chan' setting' (pack name <> ".disabled") = Nothing
            | otherwise =
                let msg = Msg {nick = nickName, channel = chan', setting = setting', message = message'}
                in case func send msg of
                    Nothing -> Nothing
                    Just a -> Just a
          plug _ _ _ = Nothing

privPlugin' :: String -> (Msg -> Maybe (Setting, IO [(Bool, Text)])) -> Plugin
privPlugin' name func = (name, plug)
    where plug :: Plug
          plug setting' send (Message (Just NickName{nickName}) "PRIVMSG" [chan', message'])
            | hasAttribute chan' setting' (pack name <> ".disabled") = Nothing
            | otherwise =
                let msg = Msg {nick = nickName, channel = chan', setting = setting', message = message'}
                in case func msg of
                    Nothing -> Nothing
                    Just (setting'', action)-> Just $ do
                        forkIO $ do
                            messages <- action
                            send $ map (\(isPrefix, x) -> (if isPrefix then privmsg else privmsgNoPref) chan' nickName x) messages
                        return setting''
          plug _ _ _ = Nothing

{-# INLINE noprefChecker #-}
noprefChecker :: (Setting -> Text -> Maybe a) -> Msg -> Maybe a
noprefChecker f Msg{setting, message} = f setting message

{-# INLINE prefChecker #-}
prefChecker :: (Setting -> Text -> Maybe a) -> Msg -> Maybe a
prefChecker f Msg{channel, setting, message} = removePrefix channel setting message >>= f setting

nopref :: (Setting -> Text -> Maybe (IO (Setting, [Text]))) -> Sender -> Msg -> Maybe (IO Setting)
nopref func send msg@Msg{channel, nick} = case noprefChecker func msg of
    Nothing -> Nothing
    Just a -> Just $ do
        (setting', msgs) <- a
        forkIO . send $ map (privmsgNoPref channel nick) msgs
        return setting'

nopref' :: (Text -> Maybe (IO [Text])) -> Sender -> Msg -> Maybe (IO Setting)
nopref' func send msg@Msg{channel, nick, setting} = case noprefChecker (const func) msg of
    Nothing -> Nothing
    Just a -> Just $ do
        forkIO $ a >>= send . map (privmsgNoPref channel nick)
        return setting

pref :: (Setting -> Text -> Maybe (IO (Setting, [Text]))) -> Sender -> Msg -> Maybe (IO Setting)
pref func send msg@Msg{channel, nick} = case prefChecker func msg of
    Nothing -> Nothing
    Just a -> Just $ do
        (setting', msgs) <- a
        forkIO . send $ map (privmsg channel nick) msgs
        return setting'

pref' :: (Text -> Maybe (IO [Text])) -> Sender -> Msg -> Maybe (IO Setting)
pref' func send msg@Msg{channel, nick, setting} = case prefChecker (const func) msg of
    Nothing -> Nothing
    Just a -> Just $ do
        forkIO $ a >>= send . map (privmsg channel nick)
        return setting


privConcat :: [Sender -> Msg -> Maybe a] -> Sender -> Msg -> Maybe a
privConcat xs send msg = headMay (mapMaybe (\f -> f send msg) xs)

regexMatch :: Regex -> Text -> Maybe [Text]
regexMatch regex x = snd <$> headMay (scan regex x)

prefix' :: (Text -> Maybe a) -> (Text -> Maybe b) -> (Either a b -> Maybe (IO [Text])) -> Sender -> Msg -> Maybe (IO Setting)
prefix' fpref fnopref func = privConcat [pref' (fpref >=> func . Left), nopref' (fnopref >=> func . Right)]

--simplePlugin :: String -> (Regex, Regex) -> (Setting -> Channel -> Text -> Either [Text] [Text] -> IO [Text]) -> Plugin
{-
simplePlugin name (pref, nopref) func = (name, plug)
  where plug :: Plug
        plug setting send (Message (Just NickName{nickName}) "PRIVMSG" [chan, msg])
            | hasAttribute chan setting (pack name <> ".disabled") = Nothing
            | otherwise =
                let withPrefix = do
                        a <- removePrefix chan setting msg
                        (Left  . snd) <$> headMay (scan pref a)
                    withoutPrefix = (Right . snd) <$> headMay (scan nopref msg)
                in (withPrefix <|> withoutPrefix) >>= \x -> Just $ do
                    forkIO $ do
                        msgs <- func setting chan nickName x
                        send $ case x of
                            Left  _ -> map (privmsg chan nickName) msgs
                            Right _ -> map (privmsgNoPref chan nickName) msgs
                    return setting
        plug _ _ _ = Nothing
        -}
