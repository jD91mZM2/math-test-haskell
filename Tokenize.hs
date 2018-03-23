module Tokenize where

import Data.Decimal

data Token = Number Decimal | Plus | Minus | Mult | Div | Factorial | GroupOpen | GroupClose
  deriving (Show, Eq)

tokenize :: [Char] -> [Token] -> [Token]
tokenize [] tokens = tokens
tokenize (c:input) tokens
  | c == ' ' = tokenize input tokens
  | c == '+' = tokenize input $ tokens ++ [Plus]
  | c == '-' = tokenize input $ tokens ++ [Minus]
  | c == '*' = tokenize input $ tokens ++ [Mult]
  | c == '/' = tokenize input $ tokens ++ [Div]
  | c == '!' = tokenize input $ tokens ++ [Factorial]
  | c == '(' = tokenize input $ tokens ++ [GroupOpen]
  | c == ')' = tokenize input $ tokens ++ [GroupClose]
  | c >= '0' && c <= '9' = do
    let predicate = (\c -> (c >= '0' && c <= '9') || c == '.')
    let num = read $ takeWhile predicate (c:input)
    let rest = dropWhile predicate input
    tokenize rest $ tokens ++ [Number num]
