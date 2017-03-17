{-# LANGUAGE PackageImports, TupleSections #-}
module Xnukbot.Plugin.Network.Misexnuk.Javascript (javascript) where

import Prelude hiding (lookup)

import "language-javascript" Language.JavaScript.Parser.AST

import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

import "unordered-containers" Data.HashMap.Lazy (HashMap, fromList, lookup, foldrWithKey, insertWith)
import qualified "containers" Data.IntMap.Lazy as I
import "containers" Data.IntMap.Lazy (IntMap)

import Control.Monad ((>=>))

import "safe" Safe (minimumByMay, readMay)
import Numeric (readFloat)
import Data.List (stripPrefix)
import Data.Function (on)

commaList :: JSCommaList a -> [a]
commaList JSLNil = []
commaList (JSLOne a) = [a]
commaList (JSLCons z _ a) = a : commaList z

varInitExp :: JSExpression -> Maybe (String, JSExpression)
varInitExp (JSVarInitExpression (JSIdentifier _ name) (JSVarInit _ expr)) = Just (name, expr)
varInitExp _ = Nothing

commaTrailingList :: JSCommaTrailingList a -> [a]
commaTrailingList (JSCTLComma x _) = commaList x
commaTrailingList (JSCTLNone x) = commaList x

propertyName :: JSPropertyName -> String
propertyName (JSPropertyIdent _ x) = x
propertyName (JSPropertyString _ x) = x
propertyName (JSPropertyNumber _ x) = x

pillMaybe :: (t1, Maybe t) -> Maybe (t1, t)
pillMaybe (a, Just b) = Just (a, b)
pillMaybe (_, Nothing) = Nothing

with :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
with f (a, b) = (a,) <$> f b

objectLiteralF :: (JSExpression -> Maybe a) -> JSExpression -> Maybe (HashMap String a)
objectLiteralF z (JSObjectLiteral _ xs _) = Just . fromList $ mapMaybe f (commaTrailingList xs)
    where f (JSPropertyNameandValue p _ [y]) = pillMaybe (propertyName p, z y)
          f _ = Nothing
objectLiteralF _ _ = Nothing

arrayLiteralF :: (JSExpression -> Maybe a) -> JSExpression -> Maybe [a]
arrayLiteralF z (JSArrayLiteral _ xs _) = Just $ mapMaybe f xs
    where f (JSArrayElement x) = z x
          f _ = Nothing
arrayLiteralF _ _ = Nothing

general :: JSExpression -> Maybe (Either Int Rational)
general (JSStringLiteral _ x) = Left <$> readMay (filter isDigit x)
general (JSDecimal _ x) =  (Right . fst) <$> minimumByMay (compare `on` (length . snd)) (readFloat x)
general (JSLiteral _ "null") = Nothing
general _ = Nothing


javascript :: JSAST -> HashMap String (IntMap Rational)
javascript (JSAstProgram xs _) = merge . fromList . concat $ mapMaybe f xs
    where f (JSVariable _ ys _) = Just . mapMaybe (with h) $ mapMaybe varInitExp (commaList ys)
          f _ = Nothing
          g = objectLiteralF . arrayLiteralF . objectLiteralF . arrayLiteralF $ objectLiteralF general
          h = g >=> lookup "rows" >=> Just . mapMaybe (lookup "c") >=> Just . I.fromList . mapMaybe i
          i :: [HashMap String (Either a b)] -> Maybe (a, b)
          i xs' = case map (lookup "v") xs' of
              [Just (Left a), Just (Right b)] -> Just (a, b)
              _ -> Nothing

javascript _ = mempty

merge :: HashMap String (IntMap Rational) -> HashMap String (IntMap Rational)
merge = flip foldrWithKey mempty $
    maybe (const id) (insertWith I.union . takeWhile (/= '_')) . stripPrefix "JSONObject"
