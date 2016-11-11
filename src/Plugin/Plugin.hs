{-# LANGUAGE PackageImports, OverloadedStrings, RecordWildCards, CPP #-}
module Plugin.Plugin (runPlugin, PluginWrapper(..)) where
import "irc" Network.IRC.Base (Message(..))
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Exception (catch, SomeException)
import System.IO (hPutStr, stderr)
import Control.Arrow (second)

import Plugin.Type (Plugin, Setting, Sender)
import "safe" Safe (headMay)

data PluginWrapper = PluginWrapper
    { setting :: MVar Setting
    , plugins :: [Plugin]
    , sender  :: Sender
    }

runPlugin :: PluginWrapper -> Message -> IO ()
runPlugin PluginWrapper{..} msg = modifyMVar_ setting $ \conf -> do
    let plugs = filter (fst . snd) $ map (second (\f -> f conf sender msg)) plugins
    case headMay plugs of
        Nothing -> return conf
        Just (name, plug) ->
#ifdef DEBUG
            snd plug -- yay early return
#else
            catch (snd plug) $ \e -> hPutStr stderr ("Plugin " ++ name ++ ": " ++ show (e :: SomeException)) >> return conf
#endif

