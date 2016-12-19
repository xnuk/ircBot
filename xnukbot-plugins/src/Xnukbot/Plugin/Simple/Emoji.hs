{-# LANGUAGE QuasiQuotes, OverloadedStrings, MultiWayIf, TupleSections #-}
module Xnukbot.Plugin.Simple.Emoji where

import Prelude hiding (lookup)

import "text" Data.Text (Text)
import "bytestring" Data.ByteString (ByteString)
import "utf8-string" Data.ByteString.UTF8 (fromString)

import "pcre-heavy" Text.Regex.PCRE.Heavy (re, Regex, (=~))

import "containers" Data.Map (Map, fromList, lookup, member)

import Control.Arrow ((***))
import Data.Monoid ((<>))

import Xnukbot.Plugin.Data.Random.Util (choice)

import "xnukbot" Xnukbot.Plugin.Types (MsgChecker, MsgMessager, makeMsgPlugin, Plugin)
import "xnukbot" Xnukbot.Plugin.Attr (hasAttribute, removePrefix)
import "xnukbot" Xnukbot.Plugin.Util (privmsgNoPrefT)

import Control.Concurrent (forkIO)

emojis :: Map ByteString Text
emojis = fromList . map (fromString *** (<> " ")) $
    [ ("d(ㅇㅅㅇ",    "ㅇㅅㅇ)b")
    , ("ㅇㅅ<",       ">ㅅㅇ")
    , ("/ㅇㅅㅇ)/",   "\\(ㅇㅅㅇ\\")
    , ("/ㅇㅅㅇ/",    "\\ㅇㅅㅇ\\")
    , ("ㅇㅅㅇ)/",    "\\(ㅇㅅㅇ")
    , ("ㅇㅅㅇ/",     "\\ㅇㅅㅇ")
    , ("\\(ㅇㅅㅇ\\", "/ㅇㅅㅇ)/")
    , ("\\ㅇㅅㅇ\\",  "/ㅇㅅㅇ/")
    , ("\\(ㅇㅅㅇ",    "ㅇㅅㅇ)/")
    , ("\\ㅇㅅㅇ",    "ㅇㅅㅇ/")

    , ("(?)", "(¿)")
    ]

regexNoPref, regexPref :: Regex
regexNoPref = [re|^\(?/?ㅇㅁㅇ\)/ㅛ\s*$|]
regexPref = [re|^(?:table(?:flip)?|flip|lenny)\s*$|]

tableFlips :: [Text]
tableFlips =
    [ "\40\9583\176\9633\176\65289\9583\65077\32\9531\9473\9531" -- (╯°□°）╯︵ ┻━┻
    , "\9531\9473\9531\32\65077\12541\40\96\1044\180\41\65417\65077\65279\32\9531\9473\9531" -- ┻━┻ ︵ヽ(`Д´)ﾉ︵﻿ ┻━┻
    , "\32\9516\9472\9516\12494\40\32\186\32\95\32\186\12494\41" -- ┬─┬ノ( º _ ºノ)
    , "\40\9583\3232\95\3248\3267\41\9583\65077\32\9531\9473\9531" -- (╯ಠ_ರೃ)╯︵ ┻━┻
    , "\40\9583\176\9633\176\65289\9583\65077\32\47\40\46\9633\46\32\92\41" -- (╯°□°）╯︵ /(.□. \)
    , "\40\9499\10061\7461\10061\65279\41\9499\24417\9531\9473\9531" -- (┛❍ᴥ❍﻿)┛彡┻━┻
    , "\40\664\8711\664\41\12463\32\24417\32\9531\9473\9531" -- (ʘ∇ʘ)ク 彡 ┻━┻
    , "\47\40\242\46\243\41\9499\24417\9531\9473\9531" -- /(ò.ó)┛彡┻━┻
    , "\40\9499\9673\1044\9673\41\9499\24417\9531\9473\9531" -- (┛◉Д◉)┛彡┻━┻
    , "\40\12494\210\30410\211\41\12494\24417\9620\9620\9615" -- (ノÒ益Ó)ノ彡▔▔▏
    ]

lenny :: Text
lenny = "( ͡° ͜ʖ ͡°)"

checker :: MsgChecker
checker setting (chan, _, msg)
    = not (hasAttribute chan setting "Emoji.disabled") &&
    (  msg =~ regexNoPref -- tableflip
    || maybe False (=~ regexPref) (removePrefix chan setting msg) -- tableflip, lenny
    || member msg emojis -- reverse emoji
    )

messager :: MsgMessager
messager setting send (chan, nick, msg) = do
    (x, setting') <-if | msg =~ [re|lenny\s*$|] -> return (lenny, setting)
                       | member msg emojis -> maybe (fail "No emoji") (return . (,setting)) $ lookup msg emojis
                       | otherwise -> choice tableFlips setting
    forkIO . send $ privmsgNoPrefT chan nick x
    return setting'

plugin :: Plugin
plugin = makeMsgPlugin "Emoji" checker messager
