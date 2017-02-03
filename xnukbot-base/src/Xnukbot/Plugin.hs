{-# LANGUAGE NoImplicitPrelude #-}
module Xnukbot.Plugin
    ( runPlugin -- Base
-- Types
    , Sender
    , Checker
    , Messager
    , Plug, Plugin, makePlugin, PluginWrapper(..)
    , AttrT(..), unAttrT, Attr, showAttr, Setting
    , MessageT(..), PrefixBiT(..)
    , Channel
-- Attr
    , getAttribute, getAttributes, hasAttribute, removePrefix
-- Util
    , privmsg, privmsgNoPref
    ) where

import Xnukbot.Plugin.Base (runPlugin)
import Xnukbot.Plugin.Types
import Xnukbot.Plugin.Attr
import Xnukbot.Plugin.Util
