{-# LANGUAGE RecordWildCards #-}
module Xnukbot.Connect.TLSConnect (tlsConnect) where

import "tls" Network.TLS (contextNew, defaultParamsClient, ClientParams(..), Supported(..), Shared(..), ValidationCache(ValidationCache), ValidationCacheResult (ValidationCachePass), handshake, Context)
import "tls" Network.TLS.Extra.Cipher (ciphersuite_all)

import "network" Network.Socket (AddrInfo(..), Socket, socket, connect, close, getAddrInfo, defaultHints, SocketType(Stream), HostName, ServiceName)

import qualified "bytestring" Data.ByteString as BS (empty)

import "data-default" Data.Default (def)

import Control.Exception (SomeException(SomeException), catch)
import Control.Monad (msum)

tlsConnect :: HostName -> ServiceName -> IO (Context, Socket)
tlsConnect hostname port = do
    addrs <- getAddrInfo (Just $ defaultHints {addrSocketType = Stream}) (Just hostname) (Just port)
    sock <- msum . flip map addrs $ \AddrInfo{..} -> do
        s <- socket addrFamily addrSocketType addrProtocol
        catch (connect s addrAddress) $ \(SomeException _) -> close s >> fail "No address was found"
        return s

    ctx <- contextNew sock $
        (defaultParamsClient hostname BS.empty)
            { clientSupported = def
                { supportedCiphers = ciphersuite_all
                , supportedSecureRenegotiation = False
                }
            , clientShared = def
                { sharedValidationCache = ValidationCache (\_ _ _ -> return ValidationCachePass) (\_ _ _ -> return ()) }
            }
    handshake ctx
    return (ctx, sock)
