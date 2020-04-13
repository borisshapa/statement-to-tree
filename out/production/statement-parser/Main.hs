module Main where

import StatementParser
import BaseParser

main :: IO ()
main = do
  str <- getLine
  putStrLn (showExpr (runParser expr str))