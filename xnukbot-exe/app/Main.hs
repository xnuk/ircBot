{-# LANGUAGE OverloadedStrings, CPP #-}

module Main where

import Prelude hiding (lookup)

import "yaml" Data.Yaml (decodeFile)

import "text" Data.Text.Encoding (encodeUtf8)
import qualified "text" Data.Text.IO as T

import Control.Monad (forever, unless)
import Data.Monoid ((<>))
import "stm" Control.Concurrent.STM (atomically)
import "stm" Control.Concurrent.STM.TQueue (writeTQueue)
import "unordered-containers" Data.HashMap.Strict (lookup)
import "unix" System.Posix.Signals (installHandler, keyboardSignal, Handler(CatchOnce), sigINT, emptySignalSet, addSignal)
import "directory" System.Directory (doesFileExist)
import Control.Concurrent (killThread, forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import "vector" Data.Vector (Vector, snoc, empty)

import "xnukbot" Xnukbot.Server.Znc (connect)
import "xnukbot" Xnukbot.IrcBot (bot)
import "xnukbot" Xnukbot.Plugin.Types (Plugin, fromSemiSetting, Config(Config))

import "xnukbot-plugins" Xnukbot.Plugin.Setting.Export (configPath)

import System.Exit (exitFailure)

#define I(Z) import qualified Xnukbot.Plugin.Z
-- I(Mueval)
I(Setting.Export)
I(Ignore)
I(Base.Setting)
I(Simple.Emoji)
I(Data.Random)
I(Network.GithubStreak)
I(Hyeong)
I(Simple.Calc)
I(Base.Echo)
I(Join.Invite)
I(Join.Part)
I(Mode.DeclineOp)
I(Base.Logger)
#undef I

plugins :: Vector Plugin
plugins = empty
#define I(Z) `snoc` Xnukbot.Plugin.Z.plugin
    -- I(Mueval)
    I(Setting.Export)
    I(Ignore)
    I(Base.Setting)
    I(Simple.Emoji)
    I(Data.Random)
    I(Network.GithubStreak)
    I(Hyeong)
    I(Simple.Calc)
    I(Base.Echo)
    I(Join.Invite)
    I(Join.Part)
    I(Mode.DeclineOp)
    I(Base.Logger)
#undef I

main :: IO ()
main = do
    path <- configPath
    fileExist <- doesFileExist path
    unless fileExist exitFailure
    Just semiSetting <- decodeFile path
    let Config (setting, conf) = fromSemiSetting semiSetting
        Just password = lookup "password" conf
        Just id' = lookup "id" conf

    (sendingQueue, byebye) <- bot (connect (encodeUtf8 id') (encodeUtf8 password)) setting plugins

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
