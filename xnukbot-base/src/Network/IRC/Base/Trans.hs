{-

Originally made by Trevor Elliott. Here's the license:

    Copyright (c) 2008 Trevor Elliott

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

License ends here.

This code also redistributed by Xnuk Shuman with BSD-3-Clause.

-}

{-# LANGUAGE NamedFieldPuns #-}
module Network.IRC.Base.Trans (PrefixT(..), MessageT(..), fromByteString, showMessage) where

import qualified "irc" Network.IRC.Base as I
import "bytestring" Data.ByteString (ByteString, singleton, empty, cons, snoc, intercalate)
import "base" Data.Monoid ((<>))
import "base" Data.Char (ord)
import "base" Data.Word (Word8)

fromChar :: Char -> Word8
fromChar = fromIntegral . ord

data PrefixT a
    = Server a
    | NickName
        { nickName :: a
        , userName :: Maybe a
        , serverName :: Maybe a
        }
    deriving Eq

instance Functor PrefixT where
    fmap f (Server a) = Server (f a)
    fmap f (NickName a b c) = NickName (f a) (f <$> b) (f <$> c)

data MessageT a = Message
    { msgPrefix :: Maybe (PrefixT a)
    , msgCommand :: a
    , msgParams :: [a]
    } deriving Eq

instance Functor MessageT where
    fmap f Message{msgPrefix, msgCommand, msgParams} = Message
        { msgPrefix = fmap f <$> msgPrefix
        , msgCommand = f msgCommand
        , msgParams = map f msgParams
        }

fromByteString :: I.Message -> MessageT ByteString
fromByteString (I.Message (Just (I.Server z)) b c) = Message (Just (Server z)) b c
fromByteString (I.Message (Just (I.NickName x y z)) b c) = Message (Just (NickName x y z)) b c
fromByteString (I.Message Nothing b c) = Message Nothing b c

showMessage :: MessageT ByteString -> ByteString
showMessage (Message p c ps) = showMaybe p <> c <> showParameters ps
  where showMaybe Nothing = empty
        showMaybe (Just prefix) = (fromChar ':' `cons` showPrefix prefix) `snoc` fromChar ' '

showPrefix :: PrefixT ByteString -> ByteString
showPrefix (Server s)       = s
showPrefix NickName{nickName, userName, serverName} =
    nickName <> consMaybe '!' userName <> consMaybe '@' serverName
    where consMaybe c = maybe empty (fromChar c `cons`)

showParameters :: [ByteString] -> ByteString
showParameters []     = empty
showParameters params = intercalate (singleton $ fromChar ' ') (empty : showp params)
  where showp [p]    = [fromChar ':' `cons` p]
        showp (p:ps) = p : showp ps
        showp []     = []
