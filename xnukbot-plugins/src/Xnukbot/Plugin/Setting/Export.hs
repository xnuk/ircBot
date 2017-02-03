{-# LANGUAGE QuasiQuotes, OverloadedStrings, NamedFieldPuns #-}
module Xnukbot.Plugin.Setting.Export (configPath, plugin) where

import Prelude hiding (writeFile, head)

import "xnukbot" Xnukbot.Plugin (Plug, Plugin, MessageT(..), PrefixBiT(NickName), removePrefix, privmsgNoPref)
import "xnukbot" Xnukbot.Plugin.Types (Config(Config), SemiSetting(semiConfig), toSemiSetting)
import "yaml" Data.Yaml (encodeFile, decodeFileEither)
import "directory" System.Directory (XdgDirectory(XdgConfig), getXdgDirectory, createDirectoryIfMissing, doesFileExist)
import "filepath" System.FilePath ((</>))
import "pcre-heavy" Text.Regex.PCRE.Heavy ((=~), re)
import qualified "unordered-containers" Data.HashMap.Strict as M
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)

configPath :: IO FilePath
configPath = do
    dir <- getXdgDirectory XdgConfig "xnukbot"
    createDirectoryIfMissing True dir
    return $ dir </> "config.yaml"

plug :: Plug
plug setting send (Message (Just (NickName nick _ host)) "PRIVMSG" [chan, msg])
    | host == Just "xnu.kr" && maybe False (=~ [re|^save\s*$|]) (removePrefix chan setting msg) = Just . (>> return setting) . forkIO $ do

        path <- configPath

        fileExist <- doesFileExist path
        semi <- if fileExist
            then either (const Nothing) id <$> decodeFileEither path
            else return Nothing
        let newSemi = toSemiSetting (Config (setting, fromMaybe M.empty (semi >>= semiConfig)))

        encodeFile path newSemi
        send [ privmsgNoPref chan nick " ✓ 저장했습니다" ]
    | otherwise = Nothing
plug _ _ _ = Nothing

plugin :: Plugin
plugin = ("SettingExport", plug)
