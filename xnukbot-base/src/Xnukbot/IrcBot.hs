{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Xnukbot.IrcBot (bot) where

import Xnukbot.Connect.Recv (ChunkFunc, loop, sendingIO)
import qualified Xnukbot.Connect.Sender as S (sender)
import Xnukbot.Plugin.Base (runPlugin)
import Xnukbot.Plugin.Types (Plugin, PluginWrapper(..), Setting)
import "bytestring" Data.ByteString (ByteString)
import "tls" Network.TLS (Context, bye)
import "stm" Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue)
import "stm" Control.Concurrent.STM (atomically)
import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import "network" Network.Socket (Socket, isConnected)
import Control.Monad (when)

bot :: (Foldable t)
    => IO ((Context, Socket), ChunkFunc)
    -> Setting
    -> t Plugin
    -> IO (TQueue ByteString, IO ())

bot connect setting' plugins = do
    setting <- newMVar setting'
    lock <- newMVar False
    sendingQueue <- atomically newTQueue

    ((ctx, sock), func) <- connect
    let close = modifyMVar_ lock $ \v -> if v then return v else do
            x <- isConnected sock
            when x $ do
                atomically $ writeTQueue sendingQueue "QUIT\r\n"
                bye ctx
            return True

        sender = S.sender sendingQueue
        plugin = PluginWrapper { setting, plugins, sender }

    ircSendThread <- forkFinally (sendingIO ctx sendingQueue) (const close)
    ircRecvThread <- forkFinally (loop (ctx, func) $ runPlugin plugin) (const close)

    return (sendingQueue, close >> killThread ircSendThread >> killThread ircRecvThread)
