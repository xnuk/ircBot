{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Attr
    ( AttrT(..), Attr, Setting
    , unAttrT, showAttr, getAttribute, getAttributes, hasAttribute, removePrefix
    ) where

import "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as B
import qualified "text" Data.Text as T
import "text" Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified "containers" Data.Map.Strict as M

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, listToMaybe)

import Xnukbot.Plugin.Types (Channel, Setting, AttrT(..), unAttrT, showAttr, Attr)
import Xnukbot.Plugin.Util (seqOr)

listAttrT :: Channel -> a -> [AttrT a]
listAttrT chan attr = map ($ attr) [Forced, Protected, Local chan, Global]

attrGet :: Channel -> Setting -> ByteString -> [ByteString]
attrGet chan setting attr = map unAttrT . f . fromMaybe [] . seqOr . map set $ listAttrT chan attr
    where f :: [AttrT a] -> [AttrT a]
          f ( x@(Forced _)   :_  ) = [x]
          f ( x@(Protected _):xs ) = x:f xs
          f ( x@(Local _ _)  :_  ) = [x]
          f ( x@(Global _)   :_  ) = [x]
          f [] = []
          set :: Attr -> Maybe (AttrT ByteString)
          set key = (<$ key) <$> M.lookup key setting

getAttribute :: Channel -> Setting -> ByteString -> Maybe ByteString
getAttribute chan setting = listToMaybe . attrGet chan setting

hasAttribute :: Channel -> Setting -> ByteString -> Bool
hasAttribute chan setting =
    any (`M.member` setting) . listAttrT chan

getAttributes :: Channel -> Setting -> ByteString -> [ByteString]
getAttributes c s = map encodeUtf8 . concatMap (T.words . decodeUtf8) . attrGet c s

removePrefix :: Channel -> Setting -> ByteString -> Maybe ByteString
removePrefix chan setting message
    | null prefixes = Nothing
    | otherwise = foldl (<|>) Nothing $ map (`B.stripPrefix` message) prefixes
    where prefixes = getAttributes chan setting "prefix"
