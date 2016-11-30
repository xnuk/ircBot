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

import "xnukbot" Xnukbot.Server.Uriirc (connect)
import "xnukbot" Xnukbot.IrcBot (bot)
import "xnukbot" Xnukbot.Plugin.Types (Plugin, Setting, AttrT(Protected, Global))

import qualified "xnukbot" Xnukbot.Plugin.Base.Echo as Echo
import qualified "xnukbot" Xnukbot.Plugin.Base.Setting as Set
import qualified "xnukbot" Xnukbot.Plugin.Base.Logger as Logger
import qualified "xnukbot-plugins" Xnukbot.Plugin.Data.Random as Random
import qualified "xnukbot-plugins" Xnukbot.Plugin.Simple.Emoji as Emoji

nick, channels :: ByteString
nick = encodeUtf8 "리덈늼"
channels = "#botworld,#botworld2"

plugins :: [Plugin]
plugins = [Emoji.plugin, Random.plugin, Set.plugin, Echo.plugin, Logger.plugin]

setting :: Setting
setting = fromList
    [ (Protected "prefix", nick <> ":")
    , (Global "prefix", "@")
    , (Protected "nickname", nick)
    ]

main :: IO ()
main = do
    (sendingQueue, byebye) <- bot connect nick channels setting plugins

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
