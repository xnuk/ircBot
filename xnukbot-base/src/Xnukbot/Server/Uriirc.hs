{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Server.Uriirc (connect) where

import "tls" Network.TLS (recvData, sendData, Context)
import Xnukbot.Connect.TLSConnect (tlsConnect)
import Xnukbot.Connect.Recv (parseit, parseMessage, ChunkFunc)
import "mtl" Control.Monad.State.Lazy (runState)
import Data.Monoid ((<>))
import "irc" Network.IRC.Base (Message(..), showMessage)
import Control.Monad.IO.Class (liftIO)
import "bytestring" Data.ByteString.Lazy (fromStrict)
import "bytestring" Data.ByteString (ByteString)
import "network" Network.Socket (Socket)

muntil :: Monad m => [m Bool] -> m Bool
muntil [] = return False
muntil (x:xs) = do
    a <- x
    if a
        then return True
        else muntil xs

connect :: ByteString -> ByteString -> IO ((Context, Socket), ChunkFunc)
connect nickname channels = do
    nctx@(ctx, _) <- tlsConnect "irc.uriirc.org" "16664"
    let z f = do
            str <- recvData ctx
            let (msgs, f') = runState (parseit str) f
            bool <- liftIO . muntil $ map q msgs
            if bool
                then return f'
                else z f'
        q msg = case msg_command msg of
            "NOTICE" | msg_params msg == ["AUTH", "*** Checking Ident"] -> do
                sendData ctx . fromStrict $ "NICK unlambdachan\r\nUSER unlambdachan xnuk xnuk :xnuk\r\n" -- no utf8 at first
                print (showMessage msg)
                return False
            "PING" -> do
                sendData ctx . fromStrict $ "PONG " <> head (msg_params msg) <> "\r\n"
                return False
            "001" -> do
                sendData ctx . fromStrict $ "NICK " <> nickname <> "\r\nUSER " <> nickname <> " xnuk xnuk :Did you know that Xnuk is so kawaii?\r\nJOIN " <> channels <> "\r\n"
                print (showMessage msg)
                return True
            _ -> return False
    f <- z parseMessage
    return (nctx, f)
