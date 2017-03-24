{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Xnukbot.Server.Znc (connect) where

import "tls" Network.TLS (recvData, sendData, Context)
import Xnukbot.Connect.TLSConnect (tlsConnect)
import Xnukbot.Connect.Recv (parseit, parseMessage, ChunkFunc)
import "mtl" Control.Monad.State.Lazy (runState)
import "irc" Network.IRC.Base (Message(..), showMessage)
import Control.Monad.IO.Class (liftIO)
import "bytestring" Data.ByteString.Lazy (fromStrict)
import "bytestring" Data.ByteString (ByteString)
import "network" Network.Socket (Socket)
import "interpolatedstring-perl6" Text.InterpolatedString.Perl6 (qq)

muntil :: Monad m => [m Bool] -> m Bool
muntil [] = return False
muntil (x:xs) = do
    a <- x
    if a
        then return True
        else muntil xs

crlf :: ByteString
crlf = "\r\n"

connect :: ByteString -> ByteString -> IO ((Context, Socket), ChunkFunc)
connect username password = do
    nctx@(ctx, _) <- tlsConnect "znc.xnu.kr" "8152"
    sendData ctx . fromStrict $ [qq|NICK $username{crlf
                                   }USER $username $username $username :$username{crlf
                                   }|]
    let z f = do
            str <- recvData ctx
            let (msgs, f') = runState (parseit str) f
            bool <- liftIO . muntil $ map login msgs
            if bool
                then return f'
                else z f'

        login msg = case msg_command msg of
            "464" | msg_params msg == [username, "Password required"] -> do
                sendData ctx . fromStrict $ [qq|PASS $username:$password{crlf}|]
                print (showMessage msg)
                return False
            "001" -> do
                print (showMessage msg)
                return True
            _ -> return False
    f <- z parseMessage
    return (nctx, f)
