{-# LANGUAGE OverloadedStrings, RecordWildCards, CPP, TupleSections #-}
module Xnukbot.Plugin.Base (makePlugin, runPlugin, PluginWrapper(..), Checker, Messager, Sender, Plugin) where

import "irc" Network.IRC.Base (Message(..))
import Control.Concurrent.MVar (modifyMVar_)
import Control.Exception (catch, SomeException)
import System.IO (hPutStr, stderr)
import Control.Applicative ((<|>))
import Data.Monoid ((<>), First(First, getFirst))
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified "text" Data.Text as T (words)
import "bytestring" Data.ByteString (stripPrefix)
import Data.Maybe (fromMaybe)

import "safe" Safe (headMay)

import Xnukbot.Plugin.Types (Sender, Checker, Messager, Plugin, makePlugin, PluginWrapper(..))
import Xnukbot.Plugin.Attr (getAttribute)

runPlugin :: Foldable t => PluginWrapper t -> Message -> IO ()
runPlugin PluginWrapper{..} msg = modifyMVar_ setting $ \conf ->
    let msg'
          | msg_command msg == "PRIVMSG" =
                let [chan, message] = msg_params msg
                    command = fmap encodeUtf8 . headMay . T.words $ decodeUtf8 message
                    alias = do
                        cmd <- command
                        newcmd <- getAttribute chan conf ("alias." <> cmd)
                        body <- stripPrefix cmd message
                        return $ msg{msg_params = [chan, newcmd <> body]}
                    aliasf = do
                        newmsg <- getAttribute chan conf ("alias!." <> message)
                        return $ msg{msg_params = [chan, newmsg]}
                in fromMaybe msg (aliasf <|> alias)
          | otherwise = msg

        headPlug = getFirst $ foldMap (\(name, f) -> First $ (name,) <$> f conf sender msg') plugins
    in case headPlug of
        Nothing -> return conf
        Just (name, plug) ->
#ifdef DEBUG
            plug -- yay early return
#else
            catch plug $ \e -> do
                hPutStr stderr ("Plugin " ++ name ++ ": " ++ show (e :: SomeException))
                return conf
#endif

