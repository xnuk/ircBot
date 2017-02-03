{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Connect.Sender (sender) where

import Xnukbot.Plugin.Types (Sender)
import Network.IRC.Base.Trans (showMessage)
import Data.Monoid ((<>))
import "stm" Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import "stm" Control.Concurrent.STM (atomically)
import "bytestring" Data.ByteString (ByteString)
import "text" Data.Text.Encoding (encodeUtf8)

sender :: TQueue ByteString -> Sender
sender tq = atomically . mapM_ (writeTQueue tq . (<> "\r\n") . showMessage . fmap encodeUtf8)
