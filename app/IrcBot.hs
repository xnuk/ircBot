{-# LANGUAGE PackageImports, OverloadedStrings, NamedFieldPuns #-}
module IrcBot (bot) where
import Recv (ChunkFunc, loop, sendingIO)
import qualified Sender as S (sender)
import Plugin.Plugin (runPlugin, PluginWrapper(..))
import Plugin.Type (Plugin, Setting, Attr(Protected))
import "bytestring" Data.ByteString (ByteString)
import "tls" Network.TLS (Context, bye)
import "stm" Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue)
import "stm" Control.Concurrent.STM (atomically)
import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import qualified "containers" Data.Map.Strict as M
import "network" Network.Socket (Socket, isConnected)
import Control.Monad (when)

bot :: (ByteString -> ByteString -> IO ((Context, Socket), ChunkFunc))
    -> ByteString -- nick
    -> ByteString -- chan
    -> Setting
    -> [Plugin]
    -> IO (TQueue ByteString, IO ())

bot connect nick chan setting' plugins = do
    setting <- newMVar $ M.insert (Protected "nickname") nick setting'
    lock <- newMVar False
    sendingQueue <- atomically newTQueue

    ((ctx, sock), func) <- connect nick chan
    let close = modifyMVar_ lock $ \v -> if v then return v else do
            x <- isConnected sock
            when x $ do
                atomically $ writeTQueue sendingQueue "QUIT\r\n"
                bye ctx
            return True

        sender = S.sender sendingQueue
        plugin = PluginWrapper { setting, plugins, sender }

    ircSendThread <- forkFinally (sendingIO ctx sendingQueue) (const close)
    ircRecvThread <- forkFinally (loop (ctx, func) $ runPlugin plugin) (const close)

    return (sendingQueue, close >> killThread ircSendThread >> killThread ircRecvThread)
