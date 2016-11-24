{-# LANGUAGE OverloadedStrings, CPP #-}

module Main where

import "text" Data.Text.Encoding (encodeUtf8)
import qualified "text" Data.Text as T
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

nick, channels :: ByteString
nick = encodeUtf8 "리덈늼"
channels = "#botworld,#botworld2"

plugins :: [Plugin]
plugins = [Random.plugin, Set.plugin, Echo.plugin, Logger.plugin]

tableFlips :: ByteString
tableFlips = encodeUtf8 . T.intercalate "," . map T.pack $
    [ "\40\9583\176\9633\176\65289\9583\65077\32\9531\9473\9531" -- (╯°□°）╯︵ ┻━┻
    , "\9531\9473\9531\32\65077\12541\40\96\1044\180\41\65417\65077\65279\32\9531\9473\9531" -- ┻━┻ ︵ヽ(`Д´)ﾉ︵﻿ ┻━┻
    , "\32\9516\9472\9516\12494\40\32\186\32\95\32\186\12494\41" -- ┬─┬ノ( º _ ºノ)
    , "\40\9583\3232\95\3248\3267\41\9583\65077\32\9531\9473\9531" -- (╯ಠ_ರೃ)╯︵ ┻━┻
    , "\40\9583\176\9633\176\65289\9583\65077\32\47\40\46\9633\46\32\92\41" -- (╯°□°）╯︵ /(.□. \)
    , "\40\9499\10061\7461\10061\65279\41\9499\24417\9531\9473\9531" -- (┛❍ᴥ❍﻿)┛彡┻━┻
    , "\40\664\8711\664\41\12463\32\24417\32\9531\9473\9531" -- (ʘ∇ʘ)ク 彡 ┻━┻
    , "\47\40\242\46\243\41\9499\24417\9531\9473\9531" -- /(ò.ó)┛彡┻━┻
    , "\40\9499\9673\1044\9673\41\9499\24417\9531\9473\9531" -- (┛◉Д◉)┛彡┻━┻
    , "\40\12494\210\30410\211\41\12494\24417\9620\9620\9615" -- (ノÒ益Ó)ノ彡▔▔▏
    ]

setting :: Setting
setting = fromList
    [ (Protected "prefix", nick <> ":")
    , (Global "prefix", "@")
    , (Protected "nickname", nick)
    , (Global "alias!.:flip", "@random," <> tableFlips)
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
