{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Util (seqOr, privmsg, privmsgNoPref, privmsgT, privmsgNoPrefT) where

import Xnukbot.Plugin.Types (Message)

import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Monoid ((<>))

import qualified "irc" Network.IRC.Commands as C (privmsg)

import "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as B

import "text" Data.Text (Text)
import "text" Data.Text.Encoding (encodeUtf8)

seqOr :: [Maybe a] -> Maybe [a]
seqOr xs = case sequence $ filter isJust xs of
    Just [] -> Nothing
    x -> x

privmsg :: ByteString -> ByteString -> ByteString -> Message
privmsg chan nick = if fromIntegral (B.head chan) == ord '#'
    then C.privmsg chan . (if nick == "" then id else ((nick <> ": ") <>))
    else C.privmsg nick

privmsgNoPref :: ByteString -> ByteString -> ByteString -> Message
privmsgNoPref chan nick = if fromIntegral (B.head chan) == ord '#'
    then C.privmsg chan
    else C.privmsg nick

privmsgT :: ByteString -> ByteString -> Text -> Message
privmsgT chan nick = privmsg chan nick . encodeUtf8

privmsgNoPrefT :: ByteString -> ByteString -> Text -> Message
privmsgNoPrefT chan nick = privmsgNoPref chan nick . encodeUtf8
