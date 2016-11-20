{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Connect.Sender (sender) where

import Xnukbot.Plugin.Base.Types (Sender)
import "irc" Network.IRC.Base (showMessage)
import Data.Monoid ((<>))
import "stm" Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import "stm" Control.Concurrent.STM (atomically)
import "bytestring" Data.ByteString (ByteString)

sender :: TQueue ByteString -> Sender
sender tq = atomically . mapM_ (writeTQueue tq . (<> "\r\n") . showMessage)
