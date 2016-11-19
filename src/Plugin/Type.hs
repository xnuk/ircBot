{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Plugin.Type (Plugin, Setting, Sender, removePrefix, getAttribute, hasAttribute, Attr(..), showAttr, makePlugin, Checker, Messager, privmsg, privmsgNoPref, privmsgT, privmsgNoPrefT) where

import "irc" Network.IRC.Base (Message)
import qualified "irc" Network.IRC.Commands as C (privmsg)
import "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as B
import "text" Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified "text" Data.Text as T
import "text" Data.Text (Text)
import "containers" Data.Map.Strict (Map)
import qualified "containers" Data.Map.Strict as M
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Control.Monad (msum)
import Data.Char (ord)

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

data Attr = Forced ByteString
          | Protected ByteString
          | Local ByteString ByteString
          | Global ByteString
    deriving (Eq, Ord, Show)

type Setting = Map Attr ByteString
type Sender = [Message] -> IO ()
type Plugin = (String, Setting -> Sender -> Message -> (Bool, IO Setting))
type Checker = Setting -> Message -> Bool
type Messager = Setting -> Sender -> Message -> IO Setting

makePlugin :: String -> Checker -> Messager -> Plugin
makePlugin name checker messager = (name, plug)
    where plug setting sender msg = (checker setting msg, messager setting sender msg)

showAttr :: Attr -> ByteString
showAttr (Global attr) = attr
showAttr (Protected attr) = "Protected " <> attr
showAttr (Local chan attr) = chan <> " " <> attr
showAttr (Forced attr) = "Forced " <> attr

attrOrder :: ByteString -> ByteString -> [Attr]
attrOrder chan attr = [Forced attr, Protected attr, Local chan attr, Global attr]

getAttribute :: ByteString -> Setting -> ByteString -> Maybe ByteString
getAttribute chan setting attr =
    foldl (<|>) Nothing . map (`M.lookup` setting) $ attrOrder chan attr

hasAttribute :: ByteString -> Setting -> ByteString -> Bool
hasAttribute chan setting attr =
    any (`M.member` setting) $ attrOrder chan attr

getAttributes :: ByteString -> Setting -> ByteString -> [ByteString]
getAttributes chan setting attr =
    msum $ map (f . flip M.lookup setting) $ attrOrder chan attr
    where f = maybe [] (map encodeUtf8 . T.words . decodeUtf8)

removePrefix :: ByteString -> Setting -> ByteString -> Maybe ByteString
removePrefix chan setting message
    | null prefixes = Nothing
    | otherwise = foldl (<|>) Nothing $ map (`B.stripPrefix` message) prefixes
    where prefixes = getAttributes chan setting "prefix"
