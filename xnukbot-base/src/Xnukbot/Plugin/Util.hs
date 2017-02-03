{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Util (seqOr, privmsg, privmsgNoPref, part, join) where

import Xnukbot.Plugin.Types (Message, MessageT(Message), Channel)

import Data.Maybe (isJust)
import Data.Monoid ((<>))

import "text" Data.Text (Text)
import qualified "text" Data.Text as T

seqOr :: [Maybe a] -> Maybe [a]
seqOr xs = case sequence $ filter isJust xs of
    Just [] -> Nothing
    x -> x

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ a = a

mkMessage :: a -> [a] -> MessageT a
mkMessage = Message Nothing

privmsg' :: Channel -> Text -> Message
privmsg' c m = mkMessage "PRIVMSG" [c, m]

privmsg :: Channel -> Text -> Text -> Message
privmsg chan nick = if T.head chan == '#'
    then privmsg' chan . if' (nick == T.empty) id ((nick <> ": ") <>)
    else privmsg' nick

privmsgNoPref :: Channel -> Text -> Text -> Message
privmsgNoPref chan nick = if T.head chan == '#'
    then privmsg' chan
    else privmsg' nick

part :: Channel -> Message
part c = mkMessage "PART" [c]

join :: Channel -> Message
join c = mkMessage "JOIN" [c]
