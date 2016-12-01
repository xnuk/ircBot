{-# LANGUAGE NoImplicitPrelude #-}
module Xnukbot.Plugin
    ( runPlugin -- Base
-- Types
    , Sender
    , Checker, MsgChecker, fromMsgChecker
    , Messager, MsgMessager, fromMsgMessager
    , Plugin, makePlugin, makeMsgPlugin, PluginWrapper(..)
    , AttrT(..), unAttrT, Attr, showAttr, Setting
    , Message(..), Prefix(..)
    , Channel
-- Attr
    , getAttribute, getAttributes, hasAttribute, removePrefix
-- Util
    , privmsg, privmsgNoPref, privmsgT, privmsgNoPrefT
    ) where

import Xnukbot.Plugin.Base (runPlugin)
import Xnukbot.Plugin.Types
import Xnukbot.Plugin.Attr
import Xnukbot.Plugin.Util
