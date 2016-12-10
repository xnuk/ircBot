{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Mueval where

import "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as B
import qualified "text" Data.Text as T
import "utf8-string" Data.ByteString.UTF8 (toString)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Control.Applicative ((<|>))

import Control.Concurrent (forkIO, killThread, threadDelay, forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, putMVar, mkWeakMVar)
import Control.Exception (catch, SomeException)

import Control.Monad (when)

import "mueval" Mueval.ArgsParse (Options(..))
import "mueval" Mueval.Interpreter (interpreter)
import "hint" Language.Haskell.Interpreter (runInterpreter, InterpreterError(..), GhcError(errMsg))

import "xnukbot" Xnukbot.Plugin.Types (MsgChecker, MsgMessager, makeMsgPlugin, Plugin, Channel, Setting)
import "xnukbot" Xnukbot.Plugin.Attr (hasAttribute, getAttribute)
import "xnukbot" Xnukbot.Plugin.Util (privmsgNoPrefT)

data Res a = Type a | Res a

option :: Options
option = Options
    { timeLimit = 5
    , modules = Just [] -- A module we should import functions from for evaluation.
    , expression = ""
    , loadFile = "" -- A local file for Mueval to load, providing definitions.
    , user = "" -- The password for the mubot account. If this is set, mueval will attempt to setuid to the mubot user.
    , printType = False
    , typeOnly = False
    , extensions = False -- Whether to enable the Glasgow extensions to Haskell '98.
    , namedExtensions = ["OverloadedStrings"]
    , noImports = True
    , rLimits = False
    , packageTrust = True
    , trustedPackages = []
    , help = False
    }

getType :: Options -> Options
getType op = op
    { printType = True
    , typeOnly = True
    }

setExp :: Options -> String -> Options
setExp op str = op { expression = str }

prefix, typePrefix :: Channel -> Setting -> ByteString
prefix     chan setting = fromMaybe ">>" $ getAttribute chan setting "Mueval.prefix"
typePrefix chan setting = fromMaybe ":t" $ getAttribute chan setting "Mueval.typePrefix"

checker :: MsgChecker
checker setting (chan, _, msg)
    = not (hasAttribute chan setting "Mueval.disabled")
    && (B.isPrefixOf prefix' msg || B.isPrefixOf typePrefix' msg)
    where prefix'     = prefix     chan setting <> " "
          typePrefix' = typePrefix chan setting <> " "

messager :: MsgMessager
messager setting send (chan, nick, msg) = forkIO run >> return setting
    where
        prefix'     = prefix     chan setting
        typePrefix' = typePrefix chan setting

        option' = fromMaybe option $
            ((getType . setExp option . toString) <$> B.stripPrefix typePrefix' msg) <|>
            ((setExp option . toString)           <$> B.stripPrefix prefix'     msg)

        send' = send . take 3 . privmsgNoPrefT chan nick

        run = do
            lock <- newEmptyMVar
            putStrLn "MuA"
            th <- forkFinally interpret (\_ -> putMVar lock ())
            putStrLn "MuB"
            threadDelay 5000000 -- it's μs!
            putStrLn "MuC"
            v <- tryTakeMVar lock
            print v
            when (isNothing v) $ do
                killThread th
                send' " ✗ Killed"
            mkWeakMVar lock (return ())
            return ()

        interpret = do
            putStrLn "MuE"
            z <- catch (runInterpreter (interpreter option')) $ \e ->
                    return . Left . UnknownError $ show (e :: SomeException)
            putStrLn "MuF"
            let s = case z of
                    Right (_, t, res) -> take 420 $ if typeOnly option' then t else res
                    Left err -> case err of
                        UnknownError str -> " ✗ Unknown>"       ++ str
                        NotAllowed   str -> " ✗ NotAllowed>"    ++ str
                        GhcException str -> " ✗ GhcException> " ++ str
                        WontCompile  xs  -> " ✗ " ++ unlines (map errMsg xs)
            send' $ T.pack s

plugin :: Plugin
plugin = makeMsgPlugin "Mueval" checker messager
