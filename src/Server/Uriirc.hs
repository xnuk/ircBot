{-# LANGUAGE PackageImports, RecordWildCards #-}
module Uriirc where

import "tls" Network.TLS (contextNew, defaultParamsClient, ClientParams(..), Supported(..), Shared(..))
import "tls" Network.TLS.Extra.Cipher (ciphersuite_all)

import "network" Network.Socket (AddrInfo(..), socket, connect, close)

import "bytestring" qualified Data.ByteString as BS (empty)

import "default" Data.Default (def)

import Control.Exception (try, SomeException)

tlsConnect hostname addr@(AddrInfo {..}) = do
    sock <- socket addrFamily addrSocketType
    conn <- try $ connect sock addr
    case conn of
      Left (SomeException _) -> close sock
      Right _ -> do
          ctx <- contextNew sock $
              (defaultParamsClient hostname BS.empty)
                  { clientSupported = def
                      { supportedCiphers = ciphersuite_all
                      }
                  }
