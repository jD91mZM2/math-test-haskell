module Parse where

import Data.Decimal
import qualified Tokenize as T

data AST =
    Number Decimal |
    VarGet String |
    VarSet String AST |

    Factorial AST  |
    Mult      AST AST |
    Div       AST AST |
    Add       AST AST |
    Sub       AST AST |
    BitShiftL AST AST |
    BitShiftR AST AST |
    BitAnd    AST AST |
    BitXor    AST AST |
    BitOr     AST AST
  deriving Show

----------------------
-- Helper functions --
----------------------

parse :: [T.Token] -> Either [Char] AST
parse tokens = do
  (tokens2, n) <- parseNum tokens
  parseLoop tokens2 n

parseLoop :: [T.Token] -> AST -> Either [Char] AST
parseLoop tokens ast = do
  (tokens2, ast2) <- parseTopLevel tokens ast
  case tokens2 of
    (T.Number n:_) -> Left $ "expected operator, found number " ++ show n
    (T.GroupOpen:_) -> do
      parseLoop (T.Mult:tokens2) ast2
    (T.GroupClose:_) -> Left "expected operator, found )"
    [] -> Right $ ast2
    _ -> do
      if tokens == tokens2
        then Left $ "stuck in infinite loop (tokens left: " ++ show tokens ++ ")"
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

applyTwo :: T.Token ->
            (AST -> AST -> AST) ->
            T.Token ->
            (AST -> AST -> AST) ->
            ([T.Token] -> AST -> Either [Char] ([T.Token], AST)) ->
            [T.Token] -> AST -> Either [Char] ([T.Token], AST)
applyTwo _ _ _ _ _ [] ast = Right ([], ast)
applyTwo token kind token2 kind2 nextFn (o:tokens) n1 = do
  if o == token || o == token2 then do
    (tokens2, tmp) <- parseNum tokens
    (tokens3, n2)  <- nextFn tokens2 tmp
    if o == token
      then Right (tokens3, kind  n1 n2)
      else Right (tokens3, kind2 n1 n2)
  else
    nextFn (o:tokens) n1

--------------------
-- Actual parsing --
--------------------

parseBitOr  = apply T.BitOr BitOr parseBitXor
parseBitXor = apply T.BitXor BitXor parseBitAnd
parseBitAnd = apply T.BitAnd BitAnd parseBitShift
parseBitShift = applyTwo T.BitShiftL BitShiftL T.BitShiftR BitShiftR parsePlus

parsePlus = applyTwo T.Plus Add T.Minus Sub parseMult
parseMult = applyTwo T.Mult Mult T.Div Div parseFac

parseFac :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseFac (T.Factorial:tokens) n = Right (tokens, Factorial n)
parseFac tokens ast = parseVar tokens ast

parseVar :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseVar (T.Eq:tokens) n = do
  case n of
    VarGet var -> do
      (tokens2, tmp) <- parseNum tokens
      (tokens3, val) <- parseTopLevel tokens2 tmp
      Right (tokens3, VarSet var val)
    t -> Left $ "assignment expected name, got " ++ show t
parseVar tokens ast = Right (tokens, ast)

parseNum :: [T.Token] -> Either [Char] ([T.Token], AST)

parseNum (T.Minus:T.Number n:tokens) = Right (tokens, Number $ -n)
parseNum (T.Number n:tokens) = Right (tokens, Number n)
parseNum (T.Text var:tokens) = Right (tokens, VarGet var)
parseNum (T.GroupOpen:tokens) = do
  (tokens2, tmp)  <- parseNum tokens
  (tokens3, ast2) <- parseTopLevel tokens2 tmp
  case tokens3 of
    [] -> Left "expected ), got EOF"
    (T.GroupClose:tokens4) -> Right (tokens4, ast2)
    (t:_) -> Left $ "expected ), got " ++ show t

parseNum (t:_) = Left $ "expected number, got " ++ show t
parseNum [] = Left "expected number, got EOF"
