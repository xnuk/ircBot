{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Xnukbot.Plugin.Data.Random (plugin) where

import "xnukbot" Xnukbot.Plugin.Types (Plugin, makePlugin, Checker, Messager, Message(Message), Prefix(NickName))
import "xnukbot" Xnukbot.Plugin.Attr (removePrefix, hasAttribute)
import "xnukbot" Xnukbot.Plugin.Util (privmsgT)

import "pcre-heavy" Text.Regex.PCRE.Heavy (re, (=~), Regex)
import "pcre-light" Text.Regex.PCRE.Light (match)

import "text" Data.Text.Encoding (decodeUtf8)
import qualified "text" Data.Text as T

import Xnukbot.Plugin.Data.Random.Util (choice)

regex :: Regex
regex = [re|^random(.)\1*(.+)$|]

checker :: Checker
checker setting (Message (Just NickName{}) "PRIVMSG" [chan, msg]) -- TODO reduce boilerplate
    = not (hasAttribute chan setting "Random.disabled") -- TODO force disabled/enabled
    && maybe False (=~ regex) (removePrefix chan setting msg)
checker _ _ = False

messager :: Messager
messager setting send (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg]) =
    case matching of
        Just (_:del:str:_) -> if null arr then return setting else do
            (x, setting') <- choice arr setting
            send [ privmsgT chan nick x ]
            return setting'
            where arr = filter (/= T.empty) $ T.split (== delim) strT
                  delim = T.head $ decodeUtf8 del
                  strT = decodeUtf8 str
        Just _ ->  fail "Regex matching failed"
        Nothing -> fail "Regex matching failed"
    where matching = do
            str <- removePrefix chan setting msg
            match regex str []
messager _ _ _ = fail "Nope"

plugin :: Plugin
plugin = makePlugin "Random" checker messager
