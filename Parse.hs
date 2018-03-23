module Parse where

import Data.Decimal
import Data.Either
import Data.Maybe
import qualified Tokenize as T

data AST =
    Number Decimal |
    Add AST AST |
    Sub AST AST |
    Mult AST AST |
    Div AST AST |
    Factorial AST
  deriving Show

parse :: [T.Token] -> Either [Char] AST
parse tokens = do
  (tokens2, n) <- parseNum tokens
  parseLoop tokens2 n

parseLoop :: [T.Token] -> AST -> Either [Char] AST
parseLoop tokens ast = do
  case tokens of
    [] -> Right ast
    _  -> do
      (tokens2, ast2) <- parsePlus tokens ast
      case tokens2 of
        (T.Number n:_) -> Left $ "expected operator, found number " ++ show n
        (T.GroupClose:_) -> Left "expected operator, found ) "
        _ -> parseLoop tokens2 ast2

parsePlus :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parsePlus [] ast = Right ([], ast)
parsePlus (o:tokens) n1 = do
  if o == T.Plus || o == T.Minus then do
    (tokens2, tmp) <- parseNum tokens
    (tokens3, n2) <- parseMult tokens2 tmp
    if o == T.Plus then
      Right (tokens3, Add n1 n2)
    else
      Right (tokens3, Sub n1 n2)
  else do
    parseMult (o:tokens) n1

parseMult :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseMult [] ast = Right ([], ast)
parseMult (o:tokens) n1 = do
  if o == T.Mult || o == T.Div then do
    (tokens2, tmp) <- parseNum tokens
    (tokens3, n2) <- parseFac tokens2 tmp
    if o == T.Mult then
      Right (tokens3, Mult n1 n2)
    else
      Right (tokens3, Div n1 n2)
  else do
    parseFac (o:tokens) n1

parseFac :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseFac [] ast = Right ([], ast)
parseFac (o:tokens) n1 = do
  if o == T.Factorial then do
    Right (tokens, Factorial n1)
  else do
    Right ((o:tokens), n1)

parseNum :: [T.Token] -> Either [Char] ([T.Token], AST)

parseNum (T.Minus:T.Number n:tokens) = Right (tokens, Number $ -n)
parseNum (T.Number n:tokens) = Right (tokens, Number n)
parseNum (T.GroupOpen:tokens) = do
  (tokens2, tmp) <- parseNum tokens
  (tokens3, ast2) <- parsePlus tokens2 tmp
  case tokens3 of
    [] -> Left "expected ), got EOF"
    (T.GroupClose:tokens4) -> Right (tokens4, ast2)
    (t:_) -> Left $ "expected ), got " ++ show t

parseNum (t:_) = Left $ "expected number, got " ++ show t
parseNum [] = Left "expected number, got EOF"
