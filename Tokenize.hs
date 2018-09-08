module Tokenize where

import Data.Char
import Data.Decimal
import Numeric

data Token =
    Number Decimal |
    GroupOpen |
    GroupClose |
    Ident String |
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
tokenize input = tokenize_ input []

tokenize_ :: [Char] -> [Token] -> Either [Char] [Token]
tokenize_ [] tokens = Right tokens
tokenize_ (' ':input) tokens     = tokenize_ input tokens

tokenize_ ('!':input) tokens     = tokenize_ input $ tokens ++ [Factorial]
tokenize_ ('*':'*':input) tokens = tokenize_ input $ tokens ++ [Pow]
tokenize_ ('*':input) tokens     = tokenize_ input $ tokens ++ [Mult]
tokenize_ ('/':input) tokens     = tokenize_ input $ tokens ++ [Div]
tokenize_ ('%':input) tokens     = tokenize_ input $ tokens ++ [Rem]
tokenize_ ('+':input) tokens     = tokenize_ input $ tokens ++ [Plus]
tokenize_ ('-':input) tokens     = tokenize_ input $ tokens ++ [Minus]
tokenize_ ('<':'<':input) tokens = tokenize_ input $ tokens ++ [BitShiftL]
tokenize_ ('>':'>':input) tokens = tokenize_ input $ tokens ++ [BitShiftR]
tokenize_ ('&':input) tokens     = tokenize_ input $ tokens ++ [BitAnd]
tokenize_ ('^':input) tokens     = tokenize_ input $ tokens ++ [BitXor]
tokenize_ ('|':input) tokens     = tokenize_ input $ tokens ++ [BitOr]

tokenize_ ('~':input) tokens = tokenize_ input $ tokens ++ [BitNot]
tokenize_ ('=':input) tokens = tokenize_ input $ tokens ++ [Eq]
tokenize_ ('(':input) tokens = tokenize_ input $ tokens ++ [GroupOpen]
tokenize_ (')':input) tokens = tokenize_ input $ tokens ++ [GroupClose]
tokenize_ ('\'':c:'\'':input) tokens = tokenize_ input $ tokens ++ [Number (fromIntegral $ ord c)]
tokenize_ (c:input) tokens
  | c >= '0' && c <= '9' = do
    case (c, length input >= 2, input) of
      ('0', True, ('x':input2)) -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
        let [(num, _)] = readHex $ takeWhile predicate input2
        tokenize_ (dropWhile predicate input2) (tokens ++ [Number num])
      ('0', True, ('o':input2)) -> do
        let predicate = (\c -> c >= '0' && c <= '7')
        let [(num, _)] = readOct $ takeWhile predicate input2
        tokenize_ (dropWhile predicate input2) (tokens ++ [Number num])
      ('0', True, ('b':input2)) -> do
        let predicate = (\c -> c == '0' || c == '1')
        let convert = (subtract $ ord '0') . ord
        let [(num, _)] = readInt 2 predicate convert $ takeWhile predicate input2
        tokenize_ (dropWhile predicate input2) (tokens ++ [Number num])
      _ -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || c == '.')
        let num = read $ takeWhile predicate (c:input)
        tokenize_ (dropWhile predicate input) (tokens ++ [Number num])
  | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = do
    let predicate = (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'));
    let ident = takeWhile predicate (c:input)
    let input2 = dropWhile predicate input
    case ident of
      "fn" -> tokenize_ input2 $ tokens ++ [Fn]
      _ -> tokenize_ input2 $ tokens ++ [Ident ident]
  | otherwise = Left $ "undefined token: " ++ show c
