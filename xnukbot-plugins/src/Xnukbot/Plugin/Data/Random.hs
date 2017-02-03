{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Xnukbot.Plugin.Data.Random (plugin) where

import "xnukbot" Xnukbot.Plugin.Types (Plugin, makePlugin, Checker, Messager, MessageT(Message), PrefixBiT(NickName))
import "xnukbot" Xnukbot.Plugin.Attr (removePrefix, hasAttribute)
import "xnukbot" Xnukbot.Plugin.Util (privmsg)

import "pcre-heavy" Text.Regex.PCRE.Heavy (re, (=~), Regex, scan)

import qualified "text" Data.Text as T

import Xnukbot.Plugin.Data.Random.Util (choice)

import Safe (headMay)

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
        Just (_, [del, str]) -> if null arr then return setting else do
            (x, setting') <- choice arr setting
            send [ privmsg chan nick x ]
            return setting'
            where arr = filter (/= T.empty) $ T.split (== T.head del) str

        Just _ ->  fail "Regex matching failed"
        Nothing -> fail "Regex matching failed"
    where matching = do
            str <- removePrefix chan setting msg
            headMay (scan regex str)
messager _ _ _ = fail "Nope"

plugin :: Plugin
plugin = makePlugin "Random" checker messager
