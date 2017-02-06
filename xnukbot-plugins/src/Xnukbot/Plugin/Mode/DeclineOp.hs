{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Plugin.Mode.DeclineOp (plugin) where

import Prelude hiding (lookup)

import "xnukbot" Xnukbot.Plugin (MessageT(Message), PrefixT(NickName), Plugin, Plug, hasAttr, AttrT(Protected))
import "xnukbot" Xnukbot.Plugin.Util (deop, voice)
import "text" Data.Text (isPrefixOf)
import "unordered-containers" Data.HashMap.Strict (lookup)
import Control.Concurrent (forkIO)
import Data.Maybe (isJust, fromJust)

plug :: Plug
plug setting send (Message (Just NickName{}) "MODE" (chan : mode : nicks))
    | hasAttr chan setting "DeclineOp.disabled" = Nothing
    | "+o" `isPrefixOf` mode && isJust nickIfMe = Just $ do
        let myNick = fromJust nickIfMe
        forkIO $ send $ voice chan [myNick] ++ deop chan [myNick]
        return setting
    | otherwise = Nothing
    where nickIfMe = case lookup (Protected "nickname") setting of
            Just nick -> if nick `elem` nicks then Just nick else Nothing
            Nothing -> Nothing

plug _ _ _ = Nothing

plugin :: Plugin
plugin = ("DeclineOp", plug)
