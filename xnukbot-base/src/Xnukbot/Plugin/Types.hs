{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards, TemplateHaskell #-}
module Xnukbot.Plugin.Types
    ( Sender
    , Checker --, MsgChecker, fromMsgChecker
    , Messager --, MsgMessager, fromMsgMessager
    , Plugin, Plug, makePlugin, {-makeMsgPlugin-} PluginWrapper(..)
    , AttrT(..), unAttrT, Attr, showAttr, Setting
    , MessageT(..), Message, PrefixT(..), Prefix
    , Channel
    , Config(Config)
    , SemiSetting(..), toSemiSetting, fromSemiSetting
    ) where

import Network.IRC.Base.Trans (MessageT(..), PrefixT(..))
import "text" Data.Text (Text)
import "unordered-containers" Data.HashMap.Strict (HashMap, foldrWithKey, union, unions, insert)
import qualified "unordered-containers" Data.HashMap.Strict as M
--import "aeson" Data.Aeson (ToJSON(toJSON), FromJSON, Value(Object))
import "aeson" Data.Aeson.TH (deriveJSON, defaultOptions, Options(omitNothingFields, fieldLabelModifier))
import "hashable" Data.Hashable (Hashable, hashWithSalt)

import Data.Char (toLower)

import Data.Monoid ((<>))
import Control.Concurrent.MVar (MVar)

type Channel = Text

type Message = MessageT Text
type Prefix = PrefixT Text

--- Plugin ---
type Checker = Setting -> Message -> Bool
type Messager = Setting -> Sender -> Message -> IO Setting
type Sender = [Message] -> IO ()
type Plug = Setting -> Sender -> Message -> Maybe (IO Setting)
type Plugin = (String, Plug)

data PluginWrapper t = PluginWrapper
    { setting :: MVar Setting
    , plugins :: t Plugin
    , sender  :: Sender
    }

makePlugin :: String -> Checker -> Messager -> Plugin
makePlugin name checker messager = (name, plug)
    where plug setting' sender' msg =
            if checker setting' msg
                then Just $ messager setting' sender' msg
                else Nothing

--- Attr ---
data AttrT a = Forced a
             | Protected a
             | Local Channel a
             | Global a
    deriving (Eq, Ord, Show)

instance Functor AttrT where
    fmap f (Forced a) = Forced (f a)
    fmap f (Protected a) = Protected (f a)
    fmap f (Local b a) = Local b (f a)
    fmap f (Global a) = Global (f a)

unAttrT :: AttrT a -> a
unAttrT (Forced a)    = a
unAttrT (Protected a) = a
unAttrT (Local _ a)   = a
unAttrT (Global a)    = a

type Attr = AttrT Text

instance (Hashable a, Monoid a) => Hashable (AttrT a) where
    hashWithSalt hash (Local chan a) = hashWithSalt hash (chan, a)
    hashWithSalt hash (Forced     a) = hashWithSalt hash ('f', a)
    hashWithSalt hash (Protected  a) = hashWithSalt hash ('p', a)
    hashWithSalt hash (Global     a) = hashWithSalt hash ('g', a)

showAttr :: Attr -> Text
showAttr (Global attr) = attr
showAttr (Protected attr) = "Protected " <> attr
showAttr (Local chan attr) = chan <> " " <> attr
showAttr (Forced attr) = "Forced " <> attr

type Setting = HashMap Attr Text

newtype Config = Config (Setting, HashMap Text Text)

data SpecSetting = SpecSetting
    { global :: HashMap Text Text
    , protected :: HashMap Text Text
    , local :: HashMap Text (HashMap Text Text)
    , forced :: HashMap Text Text
    } deriving Eq

data SemiSetting = SemiSetting
    { semiGlobal :: Maybe (HashMap Text Text)
    , semiProtected :: Maybe (HashMap Text Text)
    , semiLocal :: Maybe (HashMap Text (HashMap Text Text))
    , semiForced :: Maybe (HashMap Text Text)
    , semiConfig :: Maybe (HashMap Text Text)
    } deriving Eq

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = map toLower . drop 4} ''SemiSetting)

toSemiSetting :: Config -> SemiSetting
toSemiSetting (Config (setting', conf')) =
    let set = foldrWithKey func (SpecSetting mempty mempty mempty mempty) setting'
        func k v m = case k of
                Global    a -> m {global    = insert a v (global    m)}
                Protected a -> m {protected = insert a v (protected m)}
                Forced    a -> m {forced    = insert a v (forced    m)}
                Local chan a -> m {local = l}
                  where l = M.alter f chan (local m)
                        f Nothing  = Just $ M.singleton a v
                        f (Just x) = Just $ M.insert a v x
        z m = if M.null m then Nothing else Just m
        conv (SpecSetting a b c d) = SemiSetting (z a) (z b) (z c) (z d) (z conf')
    in conv set

fromSemiSetting :: SemiSetting -> Config
fromSemiSetting SemiSetting{..} =
    let unwrap (Just x) = x
        unwrap Nothing  = mempty
        mapKey f = foldrWithKey (M.insert . f) mempty
        setting = unions
            [ mapKey Global (unwrap semiGlobal)
            , mapKey Protected (unwrap semiProtected)
            , foldrWithKey (\k -> union . mapKey (Local k)) mempty (unwrap semiLocal)
            , mapKey Forced (unwrap semiForced)
            ]
    in Config (setting, unwrap semiConfig)
