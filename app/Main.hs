{-# LANGUAGE OverloadedStrings, NamedFieldPuns, CPP #-}

module Main where

import "xnukbot" Xnukbot.Server.Uriirc (connect)


import qualified "text" Data.Text.IO as T
import "text" Data.Text.Encoding (encodeUtf8)

import Control.Monad (forever)
import Data.Monoid ((<>))
import "stm" Control.Concurrent.STM (atomically)
import "stm" Control.Concurrent.STM.TQueue (writeTQueue)
import "containers" Data.Map.Strict (fromList)
import "unix" System.Posix.Signals (installHandler, keyboardSignal, Handler(CatchOnce), sigINT, emptySignalSet, addSignal)
import "bytestring" Data.ByteString (ByteString)
import Xnukbot.Plugin.Base.Types (Plugin, Setting, Attr(Protected, Global))
import Control.Concurrent (killThread, forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import IrcBot (bot)

import qualified Xnukbot.Plugin.Base.Echo as Echo
import qualified Xnukbot.Plugin.Base.Setting as Set
import qualified Xnukbot.Plugin.Base.Logger as Logger

nick, channels :: ByteString
nick = encodeUtf8 "리덈늼"
channels = "#botworld,#botworld2"

plugins :: [Plugin]
plugins = [Set.plugin, Echo.plugin, Logger.plugin]

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
