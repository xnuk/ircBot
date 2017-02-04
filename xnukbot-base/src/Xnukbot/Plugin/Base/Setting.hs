{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Xnukbot.Plugin.Base.Setting where

import "text" Data.Text (Text)
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re, Regex, scan)
import qualified "unordered-containers" Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Safe (headMay)

import Xnukbot.Plugin.Types
    ( MessageT(..), Message, PrefixT(NickName)
    , Plugin, Setting, Sender, AttrT(..), Attr
    , makePlugin
    )
import Xnukbot.Plugin.Attr (showAttr, getAttribute, getAttributes, removePrefix)
import Xnukbot.Plugin.Util (privmsgNoPref, privmsg)

regexSet, regexUnset, regexSuper, regexNormal, regexGet :: Regex
regexSet = [re|^set([glf]?)\s+([^\s]+)\s+(.+)$|]
regexUnset = [re|^unset([glf]?)\s+([^\s]+)\s*$|]

regexNormal = [re|^(?:un)?setl?\s+[^\s]|]
regexSuper = [re|^(?:un)?set[lgf]?\s+[^\s]|]
regexGet = [re|^[sg]et([spglf]?)\s+([^\s]+)\s*$|]

checker :: Setting -> Message -> Bool
checker setting (Message (Just (NickName _ _ host)) "PRIVMSG" [chan, msg])
    =  maybe False (=~ regexGet) msg'
    || host == Just "xnu.kr" && maybe False (=~ regexSuper) msg'
    || maybe False (=~ regexNormal) msg'
    where msg' = removePrefix chan setting msg
checker _ _ = False

messager :: Setting -> Sender -> Message -> IO Setting
messager setting send (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, mssg])
    | msg' == "" = fail "No prefix given"
    | msg' =~ regexGet = do
        send [ privmsgNoPref chan nick . fromMaybe " " $ msgGet chan setting msg' ]
        return setting
    | msg' =~ regexSet = case msgSet chan setting msg' of
        Just (attr, setting') -> do
            send [ privmsgNoPref chan nick $ " ✓ " <> showAttr attr ]
            return setting'
        Nothing -> do
            send [ privmsg chan nick " ✗ 실패하였습니다." ]
            return setting
    | msg' =~ regexUnset = case msgUnset chan setting msg' of
        Just setting' -> send [ privmsgNoPref chan nick " ✓" ] >> return setting'
        Nothing -> send [ privmsg chan nick " ✗ 이미 설정되지 않았습니다." ] >> return setting
    | otherwise = do
        send [ privmsg chan nick " ✗ 잘못된 구문입니다." ]
        return setting
    where msg' = fromMaybe "" $  removePrefix chan setting mssg
messager _ _ _ = fail "NickName only supported"

msgGet :: Text -> Setting -> Text -> Maybe Text
msgGet chan setting msg = do
    (_, [g, s]) <- headMay $ scan regexGet msg
    case g of
        "g" -> flip M.lookup setting $ Global s
        "l" -> flip M.lookup setting $ Local chan s
        "p" -> flip M.lookup setting $ Protected s
        "f" -> flip M.lookup setting $ Forced s
        "s" -> Just . mconcat . intersperse " " $ getAttributes chan setting s
        _ -> getAttribute chan setting s

msgSet :: Text -> Setting -> Text -> Maybe (Attr, Setting)
msgSet chan setting msg = do
    (_, [g, s, val]) <- headMay $ scan regexSet msg
    let attr = case g of
            "f" -> Forced s
            "g" -> Global s
            "l" -> Local chan s
            _   -> Local chan s
    return (attr, M.insert attr val setting)

msgUnset :: Text -> Setting -> Text -> Maybe Setting
msgUnset chan setting msg = do
    (_, [g, s]) <- headMay $ scan regexUnset msg
    let attr = case g of
            "f" -> Forced s
            "g" -> Global s
            "l" -> Local chan s
            _   -> Local chan s
    return $ M.delete attr setting

plugin :: Plugin
plugin = makePlugin "Setting" checker messager
