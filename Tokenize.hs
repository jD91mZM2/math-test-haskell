module Tokenize where

import Data.Char
import Data.Decimal
import Numeric

data Span = Span Int (Maybe Int)
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

splitWhile :: (Char -> Bool) -> [Char] -> ([Char], Int, [Char])
splitWhile predicate input = do
  let split = takeWhile predicate input
  let len = length split
  let remainder = drop len input
  (split, len, remainder)

tokenize :: [Char] -> Either (Span, [Char]) [(Span, Token)]
tokenize input = tokenize_ input 0 []

tokenize_ :: [Char] -> Int -> [(Span, Token)] -> Either (Span, [Char]) [(Span, Token)]
tokenize_ [] _i tokens = Right tokens
tokenize_ (' ':input) i tokens     = tokenize_ input (i+1) tokens

tokenize_ ('!':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), Factorial)]
tokenize_ ('*':'*':input) i tokens = tokenize_ input (i+2) $ tokens ++ [(Span i (Just $ i+2), Pow)]
tokenize_ ('*':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), Mult)]
tokenize_ ('/':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), Div)]
tokenize_ ('%':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), Rem)]
tokenize_ ('+':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), Plus)]
tokenize_ ('-':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), Minus)]
tokenize_ ('<':'<':input) i tokens = tokenize_ input (i+2) $ tokens ++ [(Span i (Just $ i+2), BitShiftL)]
tokenize_ ('>':'>':input) i tokens = tokenize_ input (i+2) $ tokens ++ [(Span i (Just $ i+2), BitShiftR)]
tokenize_ ('&':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), BitAnd)]
tokenize_ ('^':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), BitXor)]
tokenize_ ('|':input) i tokens     = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), BitOr)]

tokenize_ ('~':input) i tokens = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), BitNot)]
tokenize_ ('=':input) i tokens = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), Eq)]
tokenize_ ('(':input) i tokens = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), GroupOpen)]
tokenize_ (')':input) i tokens = tokenize_ input (i+1) $ tokens ++ [(Span i (Just $ i+1), GroupClose)]
tokenize_ ('\'':c:'\'':input) i tokens =
  tokenize_ input (i+3) $ tokens ++ [(Span i (Just $ i+3), Number (fromIntegral $ ord c))]
tokenize_ (c:input) i tokens
  | c >= '0' && c <= '9' = do
    case (c, length input >= 2, input) of
      ('0', True, ('x':input2)) -> do
        let predicate = (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
        let (text, len, input3) = splitWhile predicate input2
        let [(num, _)] = readHex text
        tokenize_ input3 (i+2+len) (tokens ++ [(Span (i+2) (Just $ i+2+len), Number num)])
      ('0', True, ('o':input2)) -> do
        let (text, len, input3) = splitWhile (\c -> c >= '0' && c <= '7') input2
        let [(num, _)] = readOct text
        tokenize_ input3 (i+2+len) (tokens ++ [(Span (i+2) (Just $ i+2+len), Number num)])
      ('0', True, ('b':input2)) -> do
        let convert = (subtract $ ord '0') . ord
        let (text, len, input3) = splitWhile (\c -> c == '0' || c == '1') input2
        let [(num, _)] = readInt 2 (const True) convert text
        tokenize_ input3 (i+2+len) (tokens ++ [(Span (i+2) (Just $ i+2+len), Number num)])
      _ -> do
        let (text, len, input3) = splitWhile (\c -> (c >= '0' && c <= '9') || c == '.') (c:input)
        let num = read text
        tokenize_ input3 (i+len) (tokens ++ [(Span i (Just $ i+len), Number num)])
  | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = do
    let predicate = (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'));
    let ident = takeWhile predicate (c:input)
    let input2 = dropWhile predicate input
    case ident of
      "fn" -> tokenize_ input2 (i+2) $ tokens ++ [(Span i (Just (i+2)), Fn)]
      _ -> do
        let len = length ident
        tokenize_ input2 (i+len) $ tokens ++ [(Span i (Just $ i+len), Ident ident)]
  | otherwise = Left (Span i Nothing, "undefined token: " ++ show c)
