{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Xnukbot.Server.Uriirc (connect) where

import "tls" Network.TLS (recvData, sendData, Context)
import Xnukbot.Connect.TLSConnect (tlsConnect)
import Xnukbot.Connect.Recv (parseit, parseMessage, ChunkFunc)
import "mtl" Control.Monad.State.Lazy (runState)
import Data.Monoid ((<>))
import "irc" Network.IRC.Base (Message(..), showMessage)
import Control.Monad.IO.Class (liftIO)
import "bytestring" Data.ByteString.Lazy (fromStrict)
import "bytestring" Data.ByteString (ByteString, intercalate)
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

connect :: ByteString -> [ByteString] -> IO ((Context, Socket), ChunkFunc)
connect nickname channels = do
    nctx@(ctx, _) <- tlsConnect "irc.uriirc.org" "16664"
    let z f = do
            str <- recvData ctx
            let (msgs, f') = runState (parseit str) f
            bool <- liftIO . muntil $ map q msgs
            if bool
                then return f'
                else z f'

        -- this is hard-coded login pattern.
        q msg = case msg_command msg of
            "NOTICE" | msg_params msg == ["AUTH", "*** Checking Ident"] -> do
                -- no utf8 at first
                sendData ctx . fromStrict $ "NICK unlambdachan\r\nUSER unlambdachan foobar foobar :foobar\r\n"
                print (showMessage msg)
                return False
            "PING" -> do
                sendData ctx . fromStrict $ "PONG " <> head (msg_params msg) <> "\r\n"
                return False
            "001" -> do
                sendData ctx . fromStrict $ [qq|NICK $nickname{crlf
                                               }USER $nickname foobar foobar :foobar{crlf
                                               }JOIN {intercalate "," channels}{crlf
                                               }|]
                print (showMessage msg)
                return True
            _ -> return False
    f <- z parseMessage
    return (nctx, f)
