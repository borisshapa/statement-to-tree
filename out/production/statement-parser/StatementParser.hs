module StatementParser where

import           BaseParser
import           Control.Applicative (many)
import qualified Control.Applicative as CA
import           Data.Char           (isDigit, isUpper)

upper :: Parser Char
upper = satisfy isUpper

digit :: Parser Char
digit = satisfy isDigit

symbol :: Parser Char
symbol = upper CA.<|> digit CA.<|> char '\''

data Expr
  = Disjunction Expr Expr
  | Conjunction Expr Expr
  | Implication Expr Expr
  | Variable String
  | Negative Expr
  deriving (Show)
  
showExpr :: Expr -> String
showExpr (Implication a b) = "(->," ++ showExpr a ++ "," ++ showExpr b ++ ")"
showExpr (Conjunction a b) = "(&," ++ showExpr a ++ "," ++ showExpr b ++ ")"
showExpr (Disjunction a b) = "(|," ++ showExpr a ++ "," ++ showExpr b ++ ")"
showExpr (Variable a) = a
showExpr (Negative a) = "(!" ++ showExpr a ++ ")"

variable :: Parser Expr
variable = Variable <$> (((:) <$> upper) <*> many symbol)

ws :: Parser String
ws = many (char ' ' CA.<|> char '\t')

neg :: Parser Expr
neg = ws *> ((char '!' *> (Negative <$> neg)) CA.<|> variable CA.<|> (char '(' *> expr <* char ')')) <* ws

expr :: Parser Expr
expr = ws *> implication <* ws

implication :: Parser Expr
implication = disjunction `chainr1` implOp

disjunction :: Parser Expr
disjunction = conjunction `chainl1` orOp

conjunction :: Parser Expr
conjunction = neg `chainl1` andOp

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

andOp :: Parser (Expr -> Expr -> Expr)
andOp = infixOp "&" Conjunction

orOp :: Parser (Expr -> Expr -> Expr)
orOp = infixOp "|" Disjunction

implOp :: Parser (Expr -> Expr -> Expr)
implOp = infixOp "->" Implication
