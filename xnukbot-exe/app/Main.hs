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

import "xnukbot" Xnukbot.IrcBot (bot)
import "xnukbot" Xnukbot.Plugin.Types (Plugin, fromSemiSetting, Config(Config))

import System.Exit (exitFailure)

import System.Environment (getArgs)

-- We choose ZNC, but you want to connect another server, change this.
-- For instance, we have
--
-- Xnukbot.Server.Uriirc.connect :: ByteString -> [ByteString] -> IO ((Context, Socket), ChunkFunc)
--                                   nickname       channels
--
import "xnukbot" Xnukbot.Server.Uriirc (connect)


-- This is plugin imports. If you've changed here, you probably want to change `plugins` at `main`.
#define I(Z) import qualified Xnukbot.Plugin.Z
I(Setting.Export)
I(Ignore)
I(Base.Setting)
I(Simple.Emoji)
I(Data.Random)
I(Network.GithubStreak)
I(Network.Misexnuk)
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
    -- $ xnukbot path/to/config.yaml
    [path] <- getArgs

    fileExist <- doesFileExist path
    unless fileExist exitFailure
    Just semiSetting <- decodeFile path
    let Config (setting, conf) = fromSemiSetting semiSetting
        -- Required config variables. If you think some of these are useless, just delete it.
        Just daumkey = lookup "daumkey" conf

-- This is plugin list. If you've changed here, you probably want to change plugin imports.
-- Earlier one is checked earlier, and if it says "I want this message!", the next plugins are not checked.
    let plugins :: Vector Plugin
        plugins = empty
#define I(Z) `snoc` Xnukbot.Plugin.Z.plugin
            I(Setting.Export) path
            I(Ignore)
            I(Base.Setting)
            I(Simple.Emoji)
            I(Data.Random)
            I(Network.GithubStreak)
            I(Network.Misexnuk) (encodeUtf8 daumkey)
            I(Hyeong)
            I(Simple.Calc)
            I(Base.Echo)
            I(Join.Invite)
            I(Join.Part)
            I(Mode.DeclineOp)
            I(Base.Logger)
#undef I

    -- change here if you're not going to use xnu.kr's znc
    (sendingQueue, byebye) <-
        bot (connect
                (encodeUtf8 "즈눅즈눅")
                ["#hyeon"]
            ) setting plugins

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
