{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Xnukbot.Plugin.Simple.Calc (parser, plugin) where

import "attoparsec" Data.Attoparsec.Text (Parser, parseOnly, char, string, skipWhile, takeWhile1)
import "attoparsec-expr" Data.Attoparsec.Expr (Assoc(AssocLeft), Operator(Infix, Prefix), buildExpressionParser)
import qualified "text" Data.Text as T
import "text" Data.Text (Text)
import Control.Applicative ((<|>))
import Data.Char (isDigit, isSpace)
import Data.Ratio (Rational, approxRational, (%), numerator, denominator)
import Data.Monoid ((<>))

import Control.Concurrent (forkFinally, threadDelay, killThread)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)

import "xnukbot" Xnukbot.Plugin (Plugin)
import "xnukbot" Xnukbot.Plugin.Simple (simplePlugin)

import "pcre-heavy" Text.Regex.PCRE.Heavy (re)

approx :: Double -> Rational
approx = flip approxRational (1e-20 :: Double)

exp10 :: Int -> Integer
exp10 0 = 1
exp10 1 = 10
exp10 2 = 100
exp10 3 = 1000
exp10 4 = 10000
exp10 5 = 100000
exp10 n = read $ '1' : replicate n '0'

isExp10 :: Integer -> Bool
isExp10 1 = True
isExp10 10 = True
isExp10 100 = True
isExp10 n =
    let (x:xs) = show n
    in x == '1' && all (== '0') xs

number :: Parser (Maybe Rational)
number = Just <$> (double <|> decimal <|> (string "pi" *> pure (approx pi)))

digits :: Parser String
digits = do
    str <- takeWhile1 (\c -> isSpace c || isDigit c)
    return . T.unpack $ T.filter isDigit str

decimal :: Parser Rational
decimal = (fromInteger . read) <$> digits

double :: Parser Rational
double = do
    a <- digits
    char '.'
    b <- digits
    return $ read (a ++ b) % exp10 (length b)

lift :: (a -> a -> a) -> a -> a -> Maybe a
lift f a = Just . f a

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ a = a

paren :: Parser (Maybe Rational)
paren = do
    char '('
    skipWhile isSpace
    x <- expression
    skipWhile isSpace
    char ')'
    return x

operator :: Char -> Parser ()
operator    c = skipWhile isSpace *> char   c *> skipWhile isSpace

operatorStr :: Text -> Parser ()
operatorStr s = skipWhile isSpace *> string s *> skipWhile isSpace

calcDoubleBinary :: (Double -> Double -> Double) -> Rational -> Rational -> Maybe Rational
calcDoubleBinary f a b = if' (isNaN res || isInfinite res) Nothing $ Just (approx res)
    where flat x =
            let n = numerator x
                d = denominator x
            in fromInteger n / fromInteger d
          res = f (flat a) (flat b)

calcDoubleUnary :: (Double -> Double) -> Rational -> Maybe Rational
calcDoubleUnary f a = if' (isNaN res || isInfinite res) Nothing $ Just (approx res)
    where flat =
            let n = numerator a
                d = denominator a
            in fromInteger n / fromInteger d
          res = f flat

binary :: Char -> (a -> a -> Maybe a) -> Operator Text (Maybe a)
binary c f = Infix (operator c *> pure z) AssocLeft
  where z a b = do
            x <- a
            y <- b
            f x y

prefix :: Char -> (a -> a) -> Operator Text (Maybe a)
prefix c f = Prefix (operator c *> pure (pure f <*>))

prefixDouble :: Text -> (Double -> Double) -> Operator Text (Maybe Rational)
prefixDouble s f = Prefix (operatorStr s *> pure (>>= calcDoubleUnary f))

table :: [[Operator Text (Maybe Rational)]]
table =
    [ [ prefix '-' negate, prefix '+' id ]
    , [ binary 'e' . lift $ \a b -> a * fromInteger (exp10 . fromInteger $ numerator b) ]
    , [ binary '^' $ calcDoubleBinary (**) ]
    , [ prefixDouble "exp" exp, prefixDouble "ln" log, prefixDouble "lg" (logBase 10), prefixDouble "lb" (logBase 2) ]
    , [ binary '*' $ lift (*), binary '/' $ \a b -> if b == 0 then Nothing else Just (a/b) ]
    , [ binary '+' $ lift (+), binary '-' $ lift (-) ]
    ]

expression :: Parser (Maybe Rational)
expression = buildExpressionParser table $ paren <|> number

parser :: Text -> Either String (Maybe Rational)
parser = parseOnly expression . T.strip

timeout :: Int -> IO [a] -> IO [a]
timeout ms x = do
    mvar <- newEmptyMVar
    th <- forkFinally x $ \z -> putMVar mvar $ case z of
        Left _ -> []
        Right a -> a
    forkFinally (threadDelay $ ms * 1000) $ \_ -> do
        killThread th
        putMVar mvar []

    takeMVar mvar


plugin :: Plugin
plugin = simplePlugin "Calc" ([re|^(?:(hex|dec|oct|bin)|calc (.+))$|], [re|^\s*(\(?\s*(?:-?(?:l[ngb]|exp)\s*\(?\s*\d+(?:\.\d+)?|\(?\s*(?:-?\d+(?:\.\d+)?|pi|e)\s*[\-\+\^\*/\+]).*)\s*$|]) $ \_ _ _ -> f
    where
        f x = case x of
            Right [y]    -> parseTimeout y
            Left ["", y] -> parseTimeout y
            _ -> return []

        parseTimeout = timeout 5000 . return . parse

        parse x = case parser x of
            Right (Just z) ->
                let n = numerator z
                    d = denominator z
                    ratio = T.pack (show n) <> "/" <> T.pack (show d)
                    real = T.pack (show (fromInteger n / fromInteger d :: Double))
                    output = if isExp10 d || T.length ratio > 30
                        then real
                        else real <> " = " <> ratio
                in [output]
            _ -> []


