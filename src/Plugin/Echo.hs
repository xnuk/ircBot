{-# LANGUAGE PackageImports, QuasiQuotes, OverloadedStrings #-}
module Plugin.Echo (plugin) where

import "irc" Network.IRC.Base (Message(..), Prefix(NickName))
import "irc" Network.IRC.Commands (privmsg)
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re, sub, Regex)
import qualified "containers" Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Maybe (fromJust)

import Plugin.Type (Plugin, Setting, Sender, removePrefix)

regexCmd :: Regex
regexCmd = [re|^echo |]

checker :: Setting -> Message -> Bool
checker setting (Message (Just NickName{}) "PRIVMSG" [chan, msg])
    =  not (M.member "Echo.disabled" setting)
    && not (M.member (chan <> " Echo.disabled") setting)
    && maybe False (=~ regexCmd) (removePrefix setting msg)
checker _ _ = False

messager :: Setting -> Sender -> Message -> IO Setting
messager setting send (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg]) = do
    send [ privmsg chan $ sub regexCmd (nick <> ": ") (fromJust $ removePrefix setting msg) ]
    return setting
messager _ _ _ = fail "Nickname only supported"

plugin :: Plugin
plugin =
    let func setting sender msg = (checker setting msg, messager setting sender msg)
    in ("Echo", func)

