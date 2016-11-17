{-# LANGUAGE PackageImports #-}

module Plugin.Logger where

import "irc" Network.IRC.Base (showMessage)
import qualified "text" Data.Text.IO as T (putStrLn)
import "text" Data.Text.Encoding (decodeUtf8)

import Plugin.Type (Plugin)

plugin :: Plugin
plugin = ("Logger", f)
    where f setting _ msg = (True, T.putStrLn (decodeUtf8 $ showMessage msg) >> return setting)
