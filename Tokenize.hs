module Tokenize where

import Data.Char
import Data.Decimal
import Numeric

data Token =
    Number Decimal |
    GroupOpen |
    GroupClose |

    Factorial |
    Mult |
    Div |
    Plus |
    Minus |
    BitAnd |
    BitXor |
    BitOr
  deriving (Show, Eq)

tokenize :: [Char] -> [Token] -> Either [Char] [Token]
tokenize [] tokens = Right tokens
tokenize (c:input) tokens
  | c == ' ' = tokenize input tokens
  | c == '!' = tokenize input $ tokens ++ [Factorial]
  | c == '&' = tokenize input $ tokens ++ [BitAnd]
  | c == '^' = tokenize input $ tokens ++ [BitXor]
  | c == '|' = tokenize input $ tokens ++ [BitOr]
  | c == '*' = tokenize input $ tokens ++ [Mult]
  | c == '/' = tokenize input $ tokens ++ [Div]
  | c == '+' = tokenize input $ tokens ++ [Plus]
  | c == '-' = tokenize input $ tokens ++ [Minus]

  | c == '(' = tokenize input $ tokens ++ [GroupOpen]
  | c == ')' = tokenize input $ tokens ++ [GroupClose]
  | c >= '0' && c <= '9' = do
    case (c, length input >= 2, input) of
      ('0', True, ('x':input2)) -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
        let [(num, _)] = readHex $ takeWhile predicate input2
        tokenize (dropWhile predicate input2) (tokens ++ [Number num])
      ('0', True, ('o':input2)) -> do
        let predicate = (\c -> c >= '0' && c <= '7')
        let [(num, _)] = readOct $ takeWhile predicate input2
        tokenize (dropWhile predicate input2) (tokens ++ [Number num])
      ('0', True, ('b':input2)) -> do
        let predicate = (\c -> c == '0' || c == '1')
        let convert = (\c -> ord c - ord '0')
        let [(num, _)] = readInt 2 predicate convert $ takeWhile predicate input2
        tokenize (dropWhile predicate input2) (tokens ++ [Number num])
      _ -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || c == '.')
        let num = read $ takeWhile predicate (c:input)
        tokenize (dropWhile predicate input) (tokens ++ [Number num])
  | otherwise = Left $ "undefined token: " ++ show c
