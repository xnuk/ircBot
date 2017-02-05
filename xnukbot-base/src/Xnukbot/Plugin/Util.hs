{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Util (seqOr, privmsg, privmsgNoPref, part, join, me) where

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
privmsg' c m = mkMessage "PRIVMSG" [c, T.filter (\x -> not (x == '\r' || x == '\n')) m]

withPref :: (Channel -> Text -> Message) -> Channel -> Text -> Text -> Message
withPref f chan nick = if T.head chan == '#'
    then f chan . if' (nick == T.empty) id ((nick <> ": ") <>)
    else f nick

noPref :: (Channel -> Text -> Message) -> Channel -> Text -> Text -> Message
noPref f chan nick = if T.head chan == '#'
    then f chan
    else f nick

privmsg, privmsgNoPref, me :: Channel -> Text -> Text -> Message

privmsg = withPref privmsg'
privmsgNoPref = noPref privmsg'

me c m = privmsgNoPref c $ '\1' `T.cons` "ACTION " <> m `T.snoc` '\1'

part :: Channel -> Message
part c = mkMessage "PART" [c]

join :: Channel -> Message
join c = mkMessage "JOIN" [c]
