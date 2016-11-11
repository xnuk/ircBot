{-# LANGUAGE PackageImports, OverloadedStrings, NamedFieldPuns, CPP #-}

module Main where

import Recv (loop, sendingIO)
import Server.Uriirc (connect)

import "tls" Network.TLS (bye)
import Control.Concurrent (forkFinally, killThread)

import qualified "text" Data.Text.IO as T
import "text" Data.Text.Encoding (encodeUtf8)

import Control.Monad (forever)
import Data.Monoid ((<>))
import "stm" Control.Concurrent.STM (atomically)
import "stm" Control.Concurrent.STM.TQueue (newTQueue, writeTQueue)
import Control.Concurrent.MVar (newMVar)
import "containers" Data.Map.Strict (fromList)
import Plugin.Plugin (runPlugin, PluginWrapper(..))
import qualified Plugin.Echo as Echo
import qualified Sender as S (sender)
import "unix" System.Posix.Signals (installHandler, keyboardSignal, Handler(CatchOnce), sigINT, emptySignalSet, addSignal)
import Control.Exception (catch, SomeException)
import "bytestring" Data.ByteString (ByteString)

nick, channels :: ByteString
nick = encodeUtf8 "리덈늼"
channels = "#botworld #botworld2"

main :: IO ()
main = do
    sendingQueue <- atomically newTQueue
    setting <- newMVar $ fromList
        [ ("prefix", "@")
        , ("Protected.nickname", nick)
        , ("Protected.channel", channels)
        ]

    let sender = S.sender sendingQueue
        plugins = [Echo.plugin]
        plugin = PluginWrapper { setting, plugins, sender }

    x@(ctx, _) <- connect nick channels
    let close = const (bye ctx)
    ircSendThread <- forkFinally (sendingIO ctx sendingQueue) close
    ircRecvThread <- forkFinally (loop x $ runPlugin plugin) close

    let byebye = do
            bye ctx
            killThread ircSendThread
            killThread ircRecvThread

    installHandler keyboardSignal (CatchOnce byebye) (Just $ addSignal sigINT emptySignalSet)
#ifndef DEBUG
    let lineIO = forever $ do
            line <- encodeUtf8 <$> T.getLine
            atomically $ writeTQueue sendingQueue (line <> "\r\n")
    catch lineIO $ \e -> do
        print (e :: SomeException)
        bye ctx
#endif
    return ()
