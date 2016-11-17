{-# LANGUAGE PackageImports, QuasiQuotes, OverloadedStrings #-}
module Plugin.Echo where

import "irc" Network.IRC.Base (Message(..), Prefix(NickName))
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re, sub, Regex)
import "bytestring" Data.ByteString (empty)
import Data.Maybe (fromJust)

import Plugin.Type (Plugin, Setting, Sender, removePrefix, hasAttribute, privmsg)

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
plugin =
    let func setting sender msg = (checker setting msg, messager setting sender msg)
    in ("Echo", func)

