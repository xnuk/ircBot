{-# LANGUAGE OverloadedStrings, RecordWildCards, CPP, TupleSections #-}
module Xnukbot.Plugin.Base (makePlugin, runPlugin, PluginWrapper(..), Checker, Messager, Sender, Plugin) where

import Network.IRC.Base.Trans (MessageT(..), fromByteString)
import qualified "irc" Network.IRC.Base as I
import Control.Concurrent.MVar (modifyMVar_)
import Control.Exception (catch, SomeException)
import System.IO (hPutStr, stderr)
import Control.Applicative ((<|>))
import Data.Monoid ((<>), First(First, getFirst))
import "text" Data.Text.Encoding (decodeUtf8With)
import qualified "text" Data.Text as T (words)
import "text" Data.Text (stripPrefix, Text)
import "bytestring" Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import "safe" Safe (headMay)

import Xnukbot.Plugin.Types (Sender, Checker, Messager, Plugin, makePlugin, PluginWrapper(..))
import Xnukbot.Plugin.Attr (getAttribute)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With (\_ _ -> Nothing)

runPlugin :: Foldable t => PluginWrapper t -> I.Message -> IO ()
runPlugin PluginWrapper{..} rawMsg = modifyMVar_ setting $ \conf ->
    let msgT@Message{..} = decodeUtf8 <$> fromByteString rawMsg
        msg'
          | msgCommand == "PRIVMSG" =
                let [chan, message] = msgParams
                    command = headMay $ T.words message
                    alias = do
                        cmd <- command
                        newcmd <- getAttribute chan conf ("alias." <> cmd)
                        body <- stripPrefix cmd message
                        return $ msgT {msgParams = [chan, newcmd <> body]}
                    aliasf = do
                        newmsg <- getAttribute chan conf ("alias!." <> message)
                        return $ msgT {msgParams = [chan, newmsg]}
                in fromMaybe msgT (aliasf <|> alias)
          | otherwise = msgT

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

