{-# LANGUAGE OverloadedStrings, CPP #-}

module Main where

import "text" Data.Text.Encoding (encodeUtf8)
import qualified "text" Data.Text.IO as T

import Control.Monad (forever)
import Data.Monoid ((<>))
import "stm" Control.Concurrent.STM (atomically)
import "stm" Control.Concurrent.STM.TQueue (writeTQueue)
import "containers" Data.Map.Strict (fromList)
import "unix" System.Posix.Signals (installHandler, keyboardSignal, Handler(CatchOnce), sigINT, emptySignalSet, addSignal)
import "bytestring" Data.ByteString (ByteString)
import Control.Concurrent (killThread, forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import "vector" Data.Vector (Vector, snoc, empty)

import "xnukbot" Xnukbot.Server.Uriirc (connect)
import "xnukbot" Xnukbot.IrcBot (bot)
import "xnukbot" Xnukbot.Plugin.Types (Plugin, Setting, AttrT(Protected, Global))

#define I(Z) import qualified Xnukbot.Plugin.Z
-- I(Mueval)
I(Setting.Export)
I(Simple.Emoji)
I(Data.Random)
I(Base.Setting)
I(Base.Echo)
I(Join.Invite)
I(Join.Part)
I(Base.Logger)
#undef I

plugins :: Vector Plugin
plugins = empty
#define I(Z) `snoc` Xnukbot.Plugin.Z.plugin
    -- I(Mueval)
    I(Setting.Export)
    I(Simple.Emoji)
    I(Data.Random)
    I(Base.Setting)
    I(Base.Echo)
    I(Join.Invite)
    I(Join.Part)
    I(Base.Logger)
#undef I

nick, channels :: ByteString
nick = encodeUtf8 "리덈늼"
channels = "#botworld,#botworld2"

setting :: Setting
setting = fromList
    [ (Protected "nickname", nick)
    , (Protected "prefix", nick <> ":")
    , (Global "prefix", "@")
    , (Protected "nickname", nick)
    ]

main :: IO ()
main = do
    (sendingQueue, byebye) <- bot (connect nick channels) setting plugins

#ifndef DEBUG
    lock <- newEmptyMVar
    let unlock = putMVar lock ()
    thread <- flip forkFinally (const unlock) . forever $ do
        line <- encodeUtf8 <$> T.getLine
        atomically $ writeTQueue sendingQueue (line <> "\r\n")
#endif

    _ <- installHandler keyboardSignal (CatchOnce $ do
        byebye

#ifndef DEBUG
        killThread thread
        unlock
#endif

        ) (Just $ addSignal sigINT emptySignalSet)

#ifndef DEBUG
    takeMVar lock
#else
    return ()
#endif
