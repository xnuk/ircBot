{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Xnukbot.Plugin.Setting.Export where

import Prelude hiding (writeFile, head)

import "xnukbot" Xnukbot.Plugin (Checker, MsgMessager, fromMsgMessager, makePlugin, Plugin, Message(..), Prefix(NickName), removePrefix, AttrT(..), privmsgNoPrefT)
import "directory" System.Directory (XdgDirectory(XdgConfig), getXdgDirectory, createDirectoryIfMissing)
import "filepath" System.FilePath ((</>))
import "bytestring" Data.ByteString (writeFile, head)
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re)
import qualified "containers" Data.Map.Strict as M
import Control.Concurrent (forkIO)
import Data.Char (ord)
import Data.Monoid ((<>))

checker :: Checker
checker setting (Message (Just (NickName _ _ host)) "PRIVMSG" [chan, msg])
    = host == Just "xnu.kr"
    && maybe False (=~ [re|^save\s*$|]) (removePrefix chan setting msg)
checker _ _ = False

messager :: MsgMessager
messager setting send (chan, nick, _) = (>> return setting) . forkIO $ do
    dir <- getXdgDirectory XdgConfig "xnukbot"
    createDirectoryIfMissing True dir
    writeFile (dir </> "config.rc") . (`M.foldMapWithKey` setting) $ \k v -> case k of
        Local ch  x
            | ch == "" || head ch /= fromIntegral (ord '#') -> ""
            | otherwise -> ch <> " " <> x <> " " <> v <> "\n"
        Forced    x -> "f " <> x <> " " <> v <> "\n"
        Global    x -> "g " <> x <> " " <> v <> "\n"
        Protected x -> "p " <> x <> " " <> v <> "\n"
    send $ privmsgNoPrefT chan nick " ✓ 저장했습니다"

plugin :: Plugin
plugin = makePlugin "SettingExport" checker (fromMsgMessager messager)
