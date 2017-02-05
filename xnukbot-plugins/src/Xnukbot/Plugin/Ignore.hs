{-# LANGUAGE NamedFieldPuns, OverloadedStrings, QuasiQuotes #-}
module Xnukbot.Plugin.Ignore (plugin) where

import Prelude hiding (words, unwords)

import Xnukbot.Plugin (MessageT(Message), PrefixT(NickName, nickName), hasAttribute, getAttribute, getAttributes, appendAttribute, modifyAttributes, AttrT(Local), Plug, Plugin, removePrefix, me)
import "text" Data.Text (Text, words, unwords, strip)
import "text" Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import "pcre-light" Text.Regex.PCRE.Light (compileM, utf8, caseless)
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re, scan)
import "safe" Safe (headMay)
import Data.List ((\\))
import Control.Concurrent (forkIO)

attr :: Text -> Text
attr = ("Ignore." <>)

plug :: Plug
plug setting send (Message (Just NickName{nickName}) "PRIVMSG" [chan, msg])
    | hasAttribute chan setting (attr "disabled") = Nothing
    | nickName `elem` getAttributes chan setting (attr "nick") = Just (return setting)
    | hasAttribute chan setting (attr "regex") = do
        a <- getAttribute chan setting (attr "regex")
        case compileM (encodeUtf8 a) [utf8, caseless] of
            Left _ -> Nothing
            Right regex -> if msg =~ regex
                then Just (return setting)
                else Nothing
    | otherwise = case cmd of
        Just (_, ["",   x]) -> Just $ do
            let nicks = words (strip x)
            forkIO $ send [me chan nickName $ "🔇 ignores " <> unwords nicks]
            return $ appendAttribute attrNick nicks setting
        Just (_, ["un", x]) -> Just $ do
            let nicks = words (strip x)
            forkIO $ send [me chan nickName $ "🔊 unignores " <> unwords nicks]
            return $ modifyAttributes attrNick (\\ nicks) setting
        _ -> Nothing
    where cmd = removePrefix chan setting msg >>= headMay . scan [re|^(un)?ignore\s+(.+)$|]
          attrNick = Local chan (attr "nick")
plug _ _ _ = Nothing

plugin :: Plugin
plugin = ("Ignore", plug)

