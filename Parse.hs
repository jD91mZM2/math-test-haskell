module Parse where

import Data.Decimal
import Data.Either
import Data.Maybe
import qualified Tokenize as T

data AST =
    Number Decimal |

    Factorial AST  |
    Mult   AST AST |
    Div    AST AST |
    Add    AST AST |
    Sub    AST AST |
    BitAnd AST AST |
    BitXor AST AST |
    BitOr  AST AST
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
      (tokens2, ast2) <- parseTopLevel tokens ast
      case tokens2 of
        (T.Number n:_) -> Left $ "expected operator, found number " ++ show n
        (T.GroupOpen:_) -> do
          parseLoop (T.Mult:tokens2) ast2
        (T.GroupClose:_) -> Left "expected operator, found )"
        _ -> do
          if tokens == tokens2
            then Left "stuck in infinite loop"
            else parseLoop tokens2 ast2

parseTopLevel :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseTopLevel = parseBitOr

apply :: T.Token ->
         (AST -> AST -> AST) ->
         ([T.Token] -> AST -> Either [Char] ([T.Token], AST)) ->
         [T.Token] -> AST -> Either [Char] ([T.Token], AST)
apply _ _ _ [] ast = Right ([], ast)
apply token kind nextFn (o:tokens) n1 = do
  if o == token then do
    (tokens2, tmp) <- parseNum tokens
    (tokens3, n2)  <- nextFn tokens2 tmp
    Right (tokens3, kind n1 n2)
  else
    nextFn (o:tokens) n1

parseBitOr  = apply T.BitOr BitOr parseBitXor
parseBitXor = apply T.BitXor BitXor parseBitAnd
parseBitAnd = apply T.BitAnd BitAnd parsePlus

parsePlus :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parsePlus [] ast = Right ([], ast)
parsePlus (o:tokens) n1 = do
  if o == T.Plus || o == T.Minus then do
    (tokens2, tmp) <- parseNum tokens
    (tokens3, n2)  <- parseMult tokens2 tmp
    if o == T.Plus
      then Right (tokens3, Add n1 n2)
      else Right (tokens3, Sub n1 n2)
  else
    parseMult (o:tokens) n1

parseMult :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseMult [] ast = Right ([], ast)
parseMult (o:tokens) n1 = do
  if o == T.Mult || o == T.Div then do
    (tokens2, tmp) <- parseNum tokens
    (tokens3, n2)  <- parseFac tokens2 tmp
    if o == T.Mult
      then Right (tokens3, Mult n1 n2)
      else Right (tokens3, Div n1 n2)
  else
    parseFac (o:tokens) n1

parseFac :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseFac [] ast = Right ([], ast)
parseFac (o:tokens) n1 = do
  if o == T.Factorial
    then Right (tokens, Factorial n1)
    else Right ((o:tokens), n1)

parseNum :: [T.Token] -> Either [Char] ([T.Token], AST)

parseNum (T.Minus:T.Number n:tokens) = Right (tokens, Number $ -n)
parseNum (T.Number n:tokens) = Right (tokens, Number n)
parseNum (T.GroupOpen:tokens) = do
  (tokens2, tmp)  <- parseNum tokens
  (tokens3, ast2) <- parsePlus tokens2 tmp
  case tokens3 of
    [] -> Left "expected ), got EOF"
    (T.GroupClose:tokens4) -> Right (tokens4, ast2)
    (t:_) -> Left $ "expected ), got " ++ show t

parseNum (t:_) = Left $ "expected number, got " ++ show t
parseNum [] = Left "expected number, got EOF"
