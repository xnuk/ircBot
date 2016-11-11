{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Plugin.Type (Plugin, Setting, Sender, removePrefix) where

import "irc" Network.IRC.Base (Message)
import "bytestring" Data.ByteString (ByteString)
import "text" Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified "text" Data.Text as T
import "containers" Data.Map.Strict (Map)
import qualified "containers" Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Setting = Map ByteString ByteString
type Sender = [Message] -> IO ()
type Plugin = (String, Setting -> Sender -> Message -> (Bool, IO Setting))

removePrefix :: Setting -> ByteString -> Maybe ByteString
removePrefix setting message
    | null prefixes = Nothing
    | null cand = Nothing
    | otherwise = Just . encodeUtf8 . fromMaybe "" $ T.stripPrefix (head cand) msg
    where prefixes = T.words . decodeUtf8 . fromMaybe "" $ M.lookup "prefix" setting
          msg = decodeUtf8 message
          cand = filter (`T.isPrefixOf` msg) prefixes
