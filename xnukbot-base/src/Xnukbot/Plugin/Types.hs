{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Types
    ( Sender
    , Checker, MsgChecker, fromMsgChecker
    , Messager, MsgMessager, fromMsgMessager
    , Plugin, makePlugin, makeMsgPlugin, PluginWrapper(..)
    , AttrT(..), unAttrT, Attr, showAttr, Setting
    , Message(..), Prefix(..)
    , Channel
    ) where

import "irc" Network.IRC.Base (Message(..), Prefix(..))
import "bytestring" Data.ByteString (ByteString)
import "containers" Data.Map.Strict (Map)

import Data.Monoid ((<>))
import Control.Concurrent.MVar (MVar)

type Channel = ByteString
type Nick = ByteString

--- Plugin ---
type Checker = Setting -> Message -> Bool
type MsgChecker = Setting -> (Channel, Nick, ByteString) -> Bool
type Messager = Setting -> Sender -> Message -> IO Setting
type MsgMessager = Setting -> Sender -> (Channel, Nick, ByteString) -> IO Setting
type Sender = [Message] -> IO ()
type Plugin = (String, Setting -> Sender -> Message -> Maybe (IO Setting))

data PluginWrapper t = PluginWrapper
    { setting :: MVar Setting
    , plugins :: t Plugin
    , sender  :: Sender
    }

makePlugin :: String -> Checker -> Messager -> Plugin
makePlugin name checker messager = (name, plug)
    where plug setting' sender' msg =
            if checker setting' msg
                then Just $ messager setting' sender' msg
                else Nothing

fromMsgChecker :: MsgChecker -> Checker
fromMsgChecker checker setting' (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg]) =
    checker setting' (chan, nick, msg)

fromMsgChecker _ _ _ = False

fromMsgMessager :: MsgMessager -> Messager
fromMsgMessager messager setting' send (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg]) =
    messager setting' send (chan, nick, msg)

fromMsgMessager _ _ _ _ = fail "Nope"

makeMsgPlugin :: String -> MsgChecker -> MsgMessager -> Plugin
makeMsgPlugin name checker messager = makePlugin name (fromMsgChecker checker) (fromMsgMessager messager)

--- Attr ---
data AttrT a = Forced a
             | Protected a
             | Local Channel a
             | Global a
    deriving (Eq, Ord, Show)

instance Functor AttrT where
    fmap f (Forced a) = Forced (f a)
    fmap f (Protected a) = Protected (f a)
    fmap f (Local b a) = Local b (f a)
    fmap f (Global a) = Global (f a)

unAttrT :: AttrT a -> a
unAttrT (Forced a)    = a
unAttrT (Protected a) = a
unAttrT (Local _ a)   = a
unAttrT (Global a)    = a

type Attr = AttrT ByteString

showAttr :: Attr -> ByteString
showAttr (Global attr) = attr
showAttr (Protected attr) = "Protected " <> attr
showAttr (Local chan attr) = chan <> " " <> attr
showAttr (Forced attr) = "Forced " <> attr

type Setting = Map Attr ByteString
