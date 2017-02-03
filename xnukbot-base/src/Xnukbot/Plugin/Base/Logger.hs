module Xnukbot.Plugin.Base.Logger where

import Network.IRC.Base.Trans (showMessage)
import qualified "text" Data.Text.IO as T (putStrLn)
import "text" Data.Text.Encoding (decodeUtf8With, encodeUtf8)

import Xnukbot.Plugin.Types (Plugin)

plugin :: Plugin
plugin = ("Logger", f)
    where f setting _ msg = Just (T.putStrLn (decodeUtf8With (\_ _ -> Nothing) . showMessage $ fmap encodeUtf8 msg) >> return setting)
