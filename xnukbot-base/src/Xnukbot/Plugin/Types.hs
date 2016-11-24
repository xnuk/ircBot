{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Types
    ( Sender, Checker, Messager, Plugin, makePlugin, PluginWrapper(..)
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

--- Plugin ---
type Checker = Setting -> Message -> Bool
type Messager = Setting -> Sender -> Message -> IO Setting
type Sender = [Message] -> IO ()
type Plugin = (String, Setting -> Sender -> Message -> (Bool, IO Setting))

data PluginWrapper = PluginWrapper
    { setting :: MVar Setting
    , plugins :: [Plugin]
    , sender  :: Sender
    }

makePlugin :: String -> Checker -> Messager -> Plugin
makePlugin name checker messager = (name, plug)
    where plug setting' sender' msg = (checker setting' msg, messager setting' sender' msg)

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
