{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Xnukbot.Plugin.Base.Echo where

import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re, sub, Regex)
import "bytestring" Data.ByteString (empty)
import Data.Maybe (fromJust)

import Xnukbot.Plugin.Types (Plugin, Setting, Sender, makePlugin, Message(..), Prefix(NickName))
import Xnukbot.Plugin.Attr (hasAttribute, removePrefix)
import Xnukbot.Plugin.Util (privmsg)

regexCmd :: Regex
regexCmd = [re|^echo(?:\s+|$)|]

checker :: Setting -> Message -> Bool
checker setting (Message (Just NickName{}) "PRIVMSG" [chan, msg])
    = not (hasAttribute chan setting "Echo.disabled")
    && maybe False (=~ regexCmd) (removePrefix chan setting msg)
checker _ _ = False

messager :: Setting -> Sender -> Message -> IO Setting
messager setting send (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg]) = do
    send [ privmsg chan nick $ sub regexCmd empty (fromJust $ removePrefix chan setting msg) ]
    return setting
messager _ _ _ = fail "Nickname only supported"

plugin :: Plugin
plugin = makePlugin "Echo" checker messager

