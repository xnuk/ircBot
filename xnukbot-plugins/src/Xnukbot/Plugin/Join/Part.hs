{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Xnukbot.Plugin.Join.Part where

import "xnukbot" Xnukbot.Plugin (Plug, Plugin, MessageT(Message), PrefixT(NickName), removePrefix)
import "xnukbot" Xnukbot.Plugin.Util (part)
import "pcre-heavy" Text.Regex.PCRE.Heavy (re, (=~))
import Control.Concurrent (forkIO)

plug :: Plug
plug setting send (Message (Just NickName{}) "PRIVMSG" [chan, msg])
    | maybe False (=~ [re|^part\s*$|]) (removePrefix chan setting msg) = Just $ forkIO (send [ part chan ]) >> return setting
    | otherwise = Nothing
plug _ _ _ = Nothing

plugin :: Plugin
plugin = ("Part", plug)
