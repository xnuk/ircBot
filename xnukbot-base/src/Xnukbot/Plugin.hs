{-# LANGUAGE NoImplicitPrelude #-}
module Xnukbot.Plugin
    ( runPlugin -- Base
-- Types
    , Sender
    , Checker
    , Messager
    , Plug, Plugin, makePlugin, PluginWrapper(..)
    , AttrT(..), unAttrT, Attr, showAttr, Setting
    , MessageT(..), PrefixT(..)
    , Channel
-- Attr
    , getAttribute, getAttributes, hasAttribute, setAttribute, appendAttribute, modifyAttribute, modifyAttributes
    , getAttr, getAttrs, hasAttr, setAttr, appendAttr, modifyAttr, modifyAttrs
    , removePrefix
-- Util
    , privmsg, privmsgNoPref, me
    ) where

import Xnukbot.Plugin.Base (runPlugin)
import Xnukbot.Plugin.Types
import Xnukbot.Plugin.Attr
import Xnukbot.Plugin.Util
