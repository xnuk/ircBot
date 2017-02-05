{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Attr
    ( AttrT(..), Attr, Setting
    , unAttrT, showAttr
    , getAttribute, getAttributes, hasAttribute, setAttribute, appendAttribute, modifyAttribute, modifyAttributes
    , getAttr, getAttrs, hasAttr, setAttr, appendAttr, modifyAttr, modifyAttrs
    , removePrefix
    ) where

import qualified "text" Data.Text as T
import "text" Data.Text (Text)

import qualified "unordered-containers" Data.HashMap.Strict as M

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, listToMaybe)

import Xnukbot.Plugin.Types (Channel, Setting, AttrT(..), unAttrT, showAttr, Attr)
import Xnukbot.Plugin.Util (seqOr)

listAttrT :: Channel -> a -> [AttrT a]
listAttrT chan attr = map ($ attr) [Forced, Protected, Local chan, Global]

attrGet :: Channel -> Setting -> Text -> [Text]
attrGet chan setting attr = map unAttrT . f . fromMaybe [] . seqOr . map set $ listAttrT chan attr
    where f :: [AttrT a] -> [AttrT a]
          f ( x@(Forced _)   :_  ) = [x]
          f ( x@(Protected _):xs ) = x:f xs
          f ( x@(Local _ _)  :_  ) = [x]
          f ( x@(Global _)   :_  ) = [x]
          f [] = []
          set :: Attr -> Maybe (AttrT Text)
          set key = (<$ key) <$> M.lookup key setting

getAttr, getAttribute :: Channel -> Setting -> Text -> Maybe Text
getAttr = getAttribute
getAttribute chan setting = listToMaybe . attrGet chan setting

setAttr, setAttribute :: Attr -> Text -> Setting -> Setting
setAttr = setAttribute
setAttribute = M.insert

hasAttr, hasAttribute :: Channel -> Setting -> Text -> Bool
hasAttr = hasAttribute
hasAttribute chan setting =
    any (`M.member` setting) . listAttrT chan

getAttrs, getAttributes :: Channel -> Setting -> Text -> [Text]
getAttrs = getAttributes
getAttributes c s = concatMap T.words . attrGet c s

appendAttr, appendAttribute :: Attr -> [Text] -> Setting -> Setting
appendAttr = appendAttribute
appendAttribute k vs = M.insertWith (\new old -> old `T.append` T.cons ' ' new) k (T.unwords vs)

modifyAttr, modifyAttribute :: Attr -> (Text -> Text) -> Setting -> Setting
modifyAttr = modifyAttribute
modifyAttribute k f = M.adjust f k

modifyAttrs, modifyAttributes :: Attr -> ([Text] -> [Text]) -> Setting -> Setting
modifyAttrs = modifyAttributes
modifyAttributes k f = M.adjust (T.unwords . f . T.words) k

removePrefix :: Channel -> Setting -> Text -> Maybe Text
removePrefix chan setting message
    | null prefixes = Nothing
    | otherwise = foldl (<|>) Nothing $ map (`T.stripPrefix` message) prefixes
    where prefixes = getAttributes chan setting "prefix"
