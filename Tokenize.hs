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
    BitNot |

    Factorial |
    Pow |
    Mult |
    Div |
    Rem |
    Plus |
    Minus |
    BitShiftL |
    BitShiftR |
    BitAnd |
    BitXor |
    BitOr
  deriving (Show, Eq)

tokenize :: [Char] -> Either [Char] [Token]
tokenize input = tokenize_ input True []

tokenize_ :: [Char] -> Bool -> [Token] -> Either [Char] [Token]
tokenize_ [] _ tokens = Right tokens
tokenize_ (' ':input) _ tokens     = tokenize_ input True tokens

tokenize_ ('!':input) _ tokens     = tokenize_ input True $ tokens ++ [Factorial]
tokenize_ ('*':'*':input) _ tokens = tokenize_ input True $ tokens ++ [Pow]
tokenize_ ('*':input) _ tokens     = tokenize_ input True $ tokens ++ [Mult]
tokenize_ ('/':input) _ tokens     = tokenize_ input True $ tokens ++ [Div]
tokenize_ ('%':input) _ tokens     = tokenize_ input True $ tokens ++ [Rem]
tokenize_ ('+':input) _ tokens     = tokenize_ input True $ tokens ++ [Plus]
tokenize_ ('-':input) _ tokens     = tokenize_ input True $ tokens ++ [Minus]
tokenize_ ('<':'<':input) _ tokens = tokenize_ input True $ tokens ++ [BitShiftL]
tokenize_ ('>':'>':input) _ tokens = tokenize_ input True $ tokens ++ [BitShiftR]
tokenize_ ('&':input) _ tokens     = tokenize_ input True $ tokens ++ [BitAnd]
tokenize_ ('^':input) _ tokens     = tokenize_ input True $ tokens ++ [BitXor]
tokenize_ ('|':input) _ tokens     = tokenize_ input True $ tokens ++ [BitOr]

tokenize_ ('~':input) _ tokens = tokenize_ input True $ tokens ++ [BitNot]
tokenize_ ('=':input) _ tokens = tokenize_ input True $ tokens ++ [Eq]
tokenize_ ('(':input) _ tokens = tokenize_ input True $ tokens ++ [GroupOpen]
tokenize_ (')':input) _ tokens = tokenize_ input True $ tokens ++ [GroupClose]
tokenize_ ('f':'n':input) True tokens = tokenize_ input False $ tokens ++ [Fn]
tokenize_ (c:input) space tokens
  | c >= '0' && c <= '9' = do
    case (c, length input >= 2, input) of
      ('0', True, ('x':input2)) -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
        let [(num, _)] = readHex $ takeWhile predicate input2
        tokenize_ (dropWhile predicate input2) False (tokens ++ [Number num])
      ('0', True, ('o':input2)) -> do
        let predicate = (\c -> c >= '0' && c <= '7')
        let [(num, _)] = readOct $ takeWhile predicate input2
        tokenize_ (dropWhile predicate input2) False (tokens ++ [Number num])
      ('0', True, ('b':input2)) -> do
        let predicate = (\c -> c == '0' || c == '1')
        let convert = (subtract $ ord '0') . ord
        let [(num, _)] = readInt 2 predicate convert $ takeWhile predicate input2
        tokenize_ (dropWhile predicate input2) False (tokens ++ [Number num])
      _ -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || c == '.')
        let num = read $ takeWhile predicate (c:input)
        tokenize_ (dropWhile predicate input) False (tokens ++ [Number num])
  | space && (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = do
    let predicate = (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'));
    let text = takeWhile predicate (c:input)
    let input2 = dropWhile predicate input
    tokenize_ input2 False $ tokens ++ [Text text]
  | otherwise = Left $ "undefined token: " ++ show c
