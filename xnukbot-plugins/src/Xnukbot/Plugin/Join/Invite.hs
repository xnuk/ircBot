{-# LANGUAGE OverloadedStrings #-}

module Xnukbot.Plugin.Join.Invite where

import "xnukbot" Xnukbot.Plugin (hasAttribute, Plug, Plugin, MessageT(..), PrefixT(NickName))
import "xnukbot" Xnukbot.Plugin.Util (join)

import "base" Control.Concurrent (forkIO)

plug :: Plug
plug setting send (Message (Just NickName{}) "INVITE" [_, chan])
    | hasAttribute "" setting "Invite.disabled" = Nothing
    | otherwise = Just $ forkIO (send [ join chan ]) >> return setting
plug _ _ _ = Nothing

plugin :: Plugin
plugin = ("Invite", plug)
