{-# LANGUAGE OverloadedStrings #-}

module Xnukbot.Plugin.Join.Invite where

import "xnukbot" Xnukbot.Plugin.Types (Checker, Messager, Plugin, makePlugin, Message(..), Prefix(NickName))
import "xnukbot" Xnukbot.Plugin.Attr (hasAttribute)


checker :: Checker
checker setting (Message (Just NickName{}) "INVITE" [_, _]) =
    not $ hasAttribute "" setting "Invite.disabled"
checker _ _ = False

messager :: Messager
messager setting send (Message (Just NickName{}) "INVITE" [_, chan]) = do
    send [ Message Nothing "JOIN" [chan] ]
    return setting
messager _ _ _ = fail "Nope" -- boilerplate...

plugin :: Plugin
plugin = makePlugin "Invite" checker messager
