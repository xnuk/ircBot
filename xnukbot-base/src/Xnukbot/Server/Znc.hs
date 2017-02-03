{-# LANGUAGE OverloadedStrings #-}
module Xnukbot.Server.Znc (connect) where

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
connect username password = do
    nctx@(ctx, _) <- tlsConnect "znc.xnu.kr" "8152"
    sendData ctx . fromStrict $ "NICK " <> username <> "\r\nUSER "
                              <> username <> " "
                              <> username <> " "
                              <> username <> " :" <> username
                              <> "\r\n"
    let z f = do
            str <- recvData ctx
            let (msgs, f') = runState (parseit str) f
            bool <- liftIO . muntil $ map q msgs
            if bool
                then return f'
                else z f'
        q msg = case msg_command msg of
            "464" | msg_params msg == [username, "Password required"] -> do
                sendData ctx . fromStrict $ "PASS " <> username <> ":" <> password <> "\r\n"
                print (showMessage msg)
                return False
            {-"PING" -> do
                sendData ctx . fromStrict $ "PONG " <> head (msg_params msg) <> "\r\n"
                return False-}
            "001" -> do
                print (showMessage msg)
                return True
            _ -> return False
    f <- z parseMessage
    return (nctx, f)
