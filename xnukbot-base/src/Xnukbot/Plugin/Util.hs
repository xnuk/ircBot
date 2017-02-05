{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Util (seqOr, privmsg, privmsgNoPref, part, join, me, op, deop, voice, devoice) where

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

makeOp :: Char -> Char -> Channel -> [Text] -> [Message]
makeOp _ _ _ [] = []
makeOp plus oper chan (a:b:c:d:nicks) =
    mkMessage "MODE" [chan, plus `T.cons` T.pack (replicate 4              oper), T.unwords [a, b, c, d]] : makeOp plus oper chan nicks
makeOp plus oper chan nicks = return $
    mkMessage "MODE" [chan, plus `T.cons` T.pack (replicate (length nicks) oper), T.unwords nicks]

op, deop, voice, devoice :: Channel -> [Text] -> [Message]
op      = makeOp '+' 'o'
deop    = makeOp '-' 'o'
voice   = makeOp '+' 'v'
devoice = makeOp '-' 'v'
