{-# LANGUAGE OverloadedStrings, RecordWildCards, CPP #-}
module Xnukbot.Plugin.Base (runPlugin, PluginWrapper(..)) where

import "irc" Network.IRC.Base (Message(..))
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Exception (catch, SomeException)
import System.IO (hPutStr, stderr)
import Control.Arrow (second)
import Data.Monoid ((<>))
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified "text" Data.Text as T (words)
import "bytestring" Data.ByteString (stripPrefix)
import Data.Maybe (fromMaybe)

import Xnukbot.Plugin.Base.Types (Plugin, Setting, Sender, getAttribute)
import "safe" Safe (headMay)

data PluginWrapper = PluginWrapper
    { setting :: MVar Setting
    , plugins :: [Plugin]
    , sender  :: Sender
    }

runPlugin :: PluginWrapper -> Message -> IO ()
runPlugin PluginWrapper{..} msg = modifyMVar_ setting $ \conf -> do
    let msg'
          | msg_command msg == "PRIVMSG" =
                let [chan, message] = msg_params msg
                    command = fmap encodeUtf8 . headMay . T.words $ decodeUtf8 message
                    alias = do
                        cmd <- command
                        newcmd <- getAttribute chan conf ("alias." <> cmd)
                        body <- stripPrefix cmd message
                        return $ msg{msg_params = [chan, newcmd <> body]}
                in fromMaybe msg alias
          | otherwise = msg

        plugs = filter (fst . snd) $ map (second (\f -> f conf sender msg')) plugins
    case headMay plugs of
        Nothing -> return conf
        Just (name, plug) ->
#ifdef DEBUG
            snd plug -- yay early return
#else
            catch (snd plug) $ \e -> do
                hPutStr stderr ("Plugin " ++ name ++ ": " ++ show (e :: SomeException))
                return conf
#endif

