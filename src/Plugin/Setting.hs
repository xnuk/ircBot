{-# LANGUAGE PackageImports, QuasiQuotes, OverloadedStrings #-}
module Plugin.Setting where

import "irc" Network.IRC.Base (Message(..), Prefix(NickName))
import "text" Data.Text.Encoding (encodeUtf8)
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re, Regex)
import "pcre-light" Text.Regex.PCRE.Light (match)
import qualified "containers" Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import "bytestring" Data.ByteString (ByteString)

import Plugin.Type (Plugin, Setting, Sender, removePrefix, Attr(..), getAttribute, showAttr, makePlugin, privmsgNoPref, privmsg)

regexSet, regexUnset, regexSuper, regexNormal, regexGet :: Regex
regexSet = [re|^set([glf]?)\s+([^\s]+)\s+(.+)$|]
regexUnset = [re|^unset([glf]?)\s+([^\s]+)\s*$|]

regexNormal = [re|^(?:un)?setl?\s+[^\s]|]
regexSuper = [re|^(?:un)?set[lgf]?\s+[^\s]|]
regexGet = [re|^set([pglf]?)\s+([^\s]+)\s*$|]

checker :: Setting -> Message -> Bool
checker setting (Message (Just (NickName nick _ _)) "PRIVMSG" [chan, msg])
    =  maybe False (=~ regexGet) msg'
    || nick == encodeUtf8 "즈눅" && maybe False (=~ regexSuper) msg'
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
            send [ privmsgNoPref chan nick $ encodeUtf8 " ✓ " <> showAttr attr]
            return setting'
        Nothing -> do
            send [ privmsg chan nick $ encodeUtf8 " ✗ 실패하였습니다." ]
            return setting
    | msg' =~ regexUnset = case msgUnset chan setting msg' of
        Just setting' -> send [privmsgNoPref chan nick $ encodeUtf8 " ✓"] >> return setting'
        Nothing -> send [privmsg chan nick $ encodeUtf8 " ✗ 이미 설정되지 않았습니다."] >> return setting
    | otherwise = do
        send [ privmsg chan nick $ encodeUtf8 " ✗ 잘못된 구문입니다." ]
        return setting
    where msg' = fromMaybe "" $  removePrefix chan setting mssg
messager _ _ _ = fail "NickName only supported"

msgGet :: ByteString -> Setting -> ByteString -> Maybe ByteString
msgGet chan setting msg = do
    (_:g:s:_) <- match regexGet msg []
    case g of
        "g" -> flip M.lookup setting $ Global s
        "l" -> flip M.lookup setting $ Local chan s
        "p" -> flip M.lookup setting $ Protected s
        "f" -> flip M.lookup setting $ Forced s
        _ -> getAttribute chan setting s

msgSet :: ByteString -> Setting -> ByteString -> Maybe (Attr, Setting)
msgSet chan setting msg = do
    (_:g:s:val:_) <- match regexSet msg []
    let attr = case g of
            "f" -> Forced s
            "g" -> Global s
            "l" -> Local chan s
            _   -> Local chan s
    return (attr, M.insert attr val setting)

msgUnset :: ByteString -> Setting -> ByteString -> Maybe Setting
msgUnset chan setting msg = do
    (_:g:s:_) <- match regexSet msg []
    let attr = case g of
            "f" -> Forced s
            "g" -> Global s
            "l" -> Local chan s
            _   -> Local chan s
    return $ M.delete attr setting

plugin :: Plugin
plugin = makePlugin "Setting" checker messager
