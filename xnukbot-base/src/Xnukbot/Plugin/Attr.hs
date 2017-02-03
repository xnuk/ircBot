{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Attr
    ( AttrT(..), Attr, Setting
    , unAttrT, showAttr, getAttribute, getAttributes, hasAttribute, removePrefix
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

getAttribute :: Channel -> Setting -> Text -> Maybe Text
getAttribute chan setting = listToMaybe . attrGet chan setting

hasAttribute :: Channel -> Setting -> Text -> Bool
hasAttribute chan setting =
    any (`M.member` setting) . listAttrT chan

getAttributes :: Channel -> Setting -> Text -> [Text]
getAttributes c s = concatMap T.words . attrGet c s

removePrefix :: Channel -> Setting -> Text -> Maybe Text
removePrefix chan setting message
    | null prefixes = Nothing
    | otherwise = foldl (<|>) Nothing $ map (`T.stripPrefix` message) prefixes
    where prefixes = getAttributes chan setting "prefix"
