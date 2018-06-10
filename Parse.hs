module Parse where

import Data.Decimal
import qualified Tokenize as T

data AST =
    Number Decimal |
    Negative AST |
    VarGet String |
    VarSet String AST |
    FnCall String [AST] |

    Factorial AST  |
    Pow       AST AST |
    Mult      AST AST |
    Div       AST AST |
    Rem       AST AST |
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
  (tokens2, n) <- parseIdent tokens
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

type Return = Either [Char] ([T.Token], AST)

parseTopLevel :: [T.Token] -> AST -> Return
parseTopLevel = parseBitOr

apply :: [(T.Token, (AST -> AST -> AST))] ->
            ([T.Token] -> AST -> Return) ->
            [T.Token] -> AST -> Return
apply _ _ [] ast = Right ([], ast)
apply [] nextFn tokens n1 = nextFn tokens n1
apply ((token, kind):rest) nextFn (o:tokens) n1 = do
  if o == token then do
    (tokens2, tmp) <- parseIdent tokens
    (tokens3, n2)  <- nextFn tokens2 tmp
    Right (tokens3, kind n1 n2)
  else
    apply rest nextFn (o:tokens) n1

--------------------
-- Actual parsing --
--------------------

parseBitOr = apply [(T.BitOr, BitOr), (T.BitXor, BitXor)] parseBitAnd
parseBitAnd = apply [(T.BitAnd, BitAnd)] parseBitShift
parseBitShift = apply [(T.BitShiftL, BitShiftL), (T.BitShiftR, BitShiftR)] parsePlus

parsePlus = apply [(T.Plus, Add), (T.Minus, Sub)] parseMult
parseMult = apply [(T.Mult, Mult), (T.Div, Div), (T.Rem, Rem)] parsePow
parsePow  = apply [(T.Pow, Pow)] parseFac

parseFac :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseFac (T.Factorial:tokens) n = Right (tokens, Factorial n)
parseFac tokens ast = parseVar tokens ast

parseVar :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
parseVar (T.GroupClose:tokens) ast = Right ((T.GroupClose:tokens), ast)
parseVar (T.Eq:tokens) (VarGet var) = do
  (tokens2, tmp) <- parseIdent tokens
  (tokens3, val) <- parseTopLevel tokens2 tmp
  Right (tokens3, VarSet var val)
parseVar (T.Eq:tokens) ast = Left $ "assignment expected name, got " ++ show ast
parseVar (t:tokens) (FnCall name args) = do
  (tokens2, arg) <- parseIdent (t:tokens)
  parseVar tokens2 $ FnCall name $ args ++ [arg]
parseVar tokens ast = Right (tokens, ast)

parseIdent :: [T.Token] -> Either [Char] ([T.Token], AST)

parseIdent (T.Minus:tokens) = do
  (tokens2, num) <- parseIdent tokens
  Right (tokens2, Negative num)
parseIdent (T.Number n:tokens) = Right (tokens, Number n)
parseIdent (T.Text var:tokens) = Right (tokens, VarGet var)
parseIdent (T.Fn:T.Text var:tokens) = Right (tokens, FnCall var [])
parseIdent (T.GroupOpen:tokens) = do
  (tokens2, tmp)  <- parseIdent tokens
  (tokens3, ast2) <- parseTopLevel tokens2 tmp
  case tokens3 of
    [] -> Left "expected ), got EOF"
    (T.GroupClose:tokens4) -> Right (tokens4, ast2)
    (t:_) -> Left $ "expected ), got " ++ show t

parseIdent (t:_) = Left $ "expected number, got " ++ show t
parseIdent [] = Left "expected number, got EOF"
