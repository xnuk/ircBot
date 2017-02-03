{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Xnukbot.Plugin.Base.Echo where

import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re, sub, Regex)
import "bytestring" Data.ByteString (empty)
import Data.Maybe (fromJust)

import Xnukbot.Plugin.Types (Plugin, Plug, MessageT(..), PrefixBiT(NickName))
import Xnukbot.Plugin.Attr (hasAttribute, removePrefix)
import Xnukbot.Plugin.Util (privmsg)

regexCmd :: Regex
regexCmd = [re|^echo(?:\s+|$)|]

plugin :: Plugin
plugin = ("Echo", plug)

plug :: Plug
plug setting send (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg])
    | hasAttribute chan setting "Echo.disabled" = Nothing
    | maybe False (=~ regexCmd) (removePrefix chan setting msg) = Just $ do
        send [ privmsg chan nick $ sub regexCmd empty (fromJust $ removePrefix chan setting msg) ]
        return setting
    | otherwise = Nothing

plug _ _ _ = Nothing
