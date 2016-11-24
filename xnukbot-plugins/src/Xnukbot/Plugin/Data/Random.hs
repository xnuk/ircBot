{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Xnukbot.Plugin.Data.Random (plugin) where

import "xnukbot" Xnukbot.Plugin.Types (Plugin, makePlugin, Checker, Messager, AttrT(..), Message(Message), Prefix(NickName))
import "xnukbot" Xnukbot.Plugin.Attr (removePrefix, hasAttribute)
import "xnukbot" Xnukbot.Plugin.Util (privmsgT)

import "pcre-heavy" Text.Regex.PCRE.Heavy (re, (=~), Regex)
import "pcre-light" Text.Regex.PCRE.Light (match)

import "safe" Safe (readMay)

import qualified "bytestring" Data.ByteString.Char8 as BSC

import "text" Data.Text.Encoding (decodeUtf8)
import qualified "text" Data.Text as T

import qualified "containers" Data.Map.Strict as M

import "random" System.Random (newStdGen, randomR)

regex :: Regex
regex = [re|^random(.)\1*(.+)$|]

checker :: Checker
checker setting (Message (Just NickName{}) "PRIVMSG" [chan, msg]) -- TODO reduce boilerplate
    = not (hasAttribute chan setting "Random.disabled") -- TODO force disabled/enabled
    && maybe False (=~ regex) (removePrefix chan setting msg)
checker _ _ = False

messager :: Messager
messager setting send (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg]) =
    case matching of
        Just (_:delim:str:_) -> if null arr then return setting else do
            seed <- maybe newStdGen return (M.lookup key setting >>= readMay . BSC.unpack)
            let (index, seed') = randomR (0, length arr - 1) seed
            send [ privmsgT chan nick (arr !! index) ]
            return $ M.insert key (BSC.pack $ show seed') setting
            where arr = filter (/= T.empty) $ T.split (== T.head (decodeUtf8 delim)) (decodeUtf8 str)
        Just _ ->  fail "Regex matching failed"
        Nothing -> fail "Regex matching failed"
    where matching = do
            str <- removePrefix chan setting msg
            match regex str []
          key = Protected "Random.seed"
messager _ _ _ = fail "Nope"

plugin :: Plugin
plugin = makePlugin "Random" checker messager
