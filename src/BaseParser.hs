{-# LANGUAGE LambdaCase #-}

module BaseParser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List           (isPrefixOf)

newtype Parser a =
  Parser
    { parse :: String -> [(a, String)]
    }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)]    -> error "Parser did not consume entire string."
    _           -> error "Parser error."

item :: Parser Char
item =
  Parser $ \case
    [] -> []
    (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=) = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (const [])

option :: Parser a -> Parser a -> Parser a
option p q =
  Parser $ \s ->
    case parse p s of
      []  -> parse q s
      res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  item `bind` \c ->
    if p c
      then unit c
      else failure

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a =
      do f <- op
         b <- p
         rest (f a b)
     <|> return a

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (p `chainl1` op) <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan = do
      a <- p
      rest a
    rest a =
      do f <- op
         f a <$> scan
     <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c : cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

prefix :: String -> Parser String
prefix s = Parser f
  where
    f input = [(drop (length s) input, s) | s `isPrefixOf` input]

skipString :: String -> Parser ()
skipString s = () <$ prefix s