module Tokenize where

import Data.Char
import Data.Decimal
import Numeric

data Token =
    Number Decimal |
    GroupOpen |
    GroupClose |
    Text String |
    Eq |
    Fn |

    Factorial |
    Pow |
    Mult |
    Div |
    Plus |
    Minus |
    BitShiftL |
    BitShiftR |
    BitAnd |
    BitXor |
    BitOr
  deriving (Show, Eq)

tokenize :: [Char] -> Bool -> [Token] -> Either [Char] [Token]
tokenize [] _ tokens = Right tokens
tokenize (' ':input) _ tokens     = tokenize input True tokens

tokenize ('!':input) _ tokens     = tokenize input True $ tokens ++ [Factorial]
tokenize ('*':'*':input) _ tokens = tokenize input True $ tokens ++ [Pow]
tokenize ('*':input) _ tokens     = tokenize input True $ tokens ++ [Mult]
tokenize ('/':input) _ tokens     = tokenize input True $ tokens ++ [Div]
tokenize ('+':input) _ tokens     = tokenize input True $ tokens ++ [Plus]
tokenize ('-':input) _ tokens     = tokenize input True $ tokens ++ [Minus]
tokenize ('<':'<':input) _ tokens = tokenize input True $ tokens ++ [BitShiftL]
tokenize ('>':'>':input) _ tokens = tokenize input True $ tokens ++ [BitShiftR]
tokenize ('&':input) _ tokens     = tokenize input True $ tokens ++ [BitAnd]
tokenize ('^':input) _ tokens     = tokenize input True $ tokens ++ [BitXor]
tokenize ('|':input) _ tokens     = tokenize input True $ tokens ++ [BitOr]

tokenize ('=':input) _ tokens = tokenize input True $ tokens ++ [Eq]
tokenize ('(':input) _ tokens = tokenize input True $ tokens ++ [GroupOpen]
tokenize (')':input) _ tokens = tokenize input True $ tokens ++ [GroupClose]
tokenize ('f':'n':input) True tokens = tokenize input False $ tokens ++ [Fn]
tokenize (c:input) space tokens
  | c >= '0' && c <= '9' = do
    case (c, length input >= 2, input) of
      ('0', True, ('x':input2)) -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
        let [(num, _)] = readHex $ takeWhile predicate input2
        tokenize (dropWhile predicate input2) False (tokens ++ [Number num])
      ('0', True, ('o':input2)) -> do
        let predicate = (\c -> c >= '0' && c <= '7')
        let [(num, _)] = readOct $ takeWhile predicate input2
        tokenize (dropWhile predicate input2) False (tokens ++ [Number num])
      ('0', True, ('b':input2)) -> do
        let predicate = (\c -> c == '0' || c == '1')
        let convert = (subtract $ ord '0') . ord
        let [(num, _)] = readInt 2 predicate convert $ takeWhile predicate input2
        tokenize (dropWhile predicate input2) False (tokens ++ [Number num])
      _ -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || c == '.')
        let num = read $ takeWhile predicate (c:input)
        tokenize (dropWhile predicate input) False (tokens ++ [Number num])
  | space && (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = do
    let predicate = (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'));
    let text = takeWhile predicate (c:input)
    let input2 = dropWhile predicate input
    tokenize input2 False $ tokens ++ [Text text]
  | otherwise = Left $ "undefined token: " ++ show c
