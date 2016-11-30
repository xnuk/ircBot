{-# LANGUAGE QuasiQuotes #-}

module Xnukbot.Plugin.Join.Part where

import "xnukbot" Xnukbot.Plugin.Types (MsgChecker, MsgMessager, makeMsgPlugin, Plugin)
import "xnukbot" Xnukbot.Plugin (removePrefix)

import "irc" Network.IRC.Commands (part)

import "pcre-heavy" Text.Regex.PCRE.Heavy (re, (=~))

import Control.Concurrent (forkIO)

checker :: MsgChecker
checker setting (chan, _, msg) = maybe False (=~ [re|^part\s*$|]) (removePrefix chan setting msg)

messager :: MsgMessager
messager setting send (chan, _, _) = forkIO (send [ part chan ]) >> return setting

plugin :: Plugin
plugin = makeMsgPlugin "Part" checker messager
