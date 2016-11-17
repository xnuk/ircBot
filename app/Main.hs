{-# LANGUAGE PackageImports, OverloadedStrings, NamedFieldPuns, CPP #-}

module Main where

import Server.Uriirc (connect)


import qualified "text" Data.Text.IO as T
import "text" Data.Text.Encoding (encodeUtf8)

import Control.Monad (forever)
import Data.Monoid ((<>))
import "stm" Control.Concurrent.STM (atomically)
import "stm" Control.Concurrent.STM.TQueue (writeTQueue)
import "containers" Data.Map.Strict (fromList)
import "unix" System.Posix.Signals (installHandler, keyboardSignal, Handler(CatchOnce), sigINT, emptySignalSet, addSignal)
import Control.Exception (catch, SomeException)
import "bytestring" Data.ByteString (ByteString)
import Plugin.Type (Plugin, Setting, Attr(Protected, Global))

import IrcBot (bot)


import qualified Plugin.Echo as Echo
import qualified Plugin.Setting as Set
import qualified Plugin.Logger as Logger

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
    (sendingQueue, byeby) <- bot connect nick channels setting plugins
    let byebye = putStrLn ">close<" >> byeby
    installHandler keyboardSignal (CatchOnce byebye) (Just $ addSignal sigINT emptySignalSet)
#ifndef DEBUG
    let lineIO = forever $ do
            line <- encodeUtf8 <$> T.getLine
            atomically $ writeTQueue sendingQueue (line <> "\r\n")
    catch lineIO $ \e -> do
        putStrLn $ "UI: " ++ show (e :: SomeException)
        byebye
#endif
    return ()
