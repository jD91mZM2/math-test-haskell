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
  (tokens3, n2) <- parseInner tokens2 n True
  if not $ tokens3 == []
    then Left "tokens aren't empty after parsing correctly. this is a bug."
    else Right n2

parseInner :: [T.Token] -> AST -> Bool -> Either [Char] ([T.Token], AST)
parseInner tokens ast toplevel = do
  (tokens2, ast2) <- parseTopLevel tokens ast
  case tokens2 of
    (T.Number n:_) -> Left $ "expected operator, found number " ++ show n
    (T.GroupClose:_) -> do
      if toplevel
        then Left "expected operator, found )"
        else Right (tokens2, ast2)
    [] -> Right (tokens2, ast2)
    _ -> do
      Left $ "trailing input was not parsed: " ++ show tokens

type Return = Either [Char] ([T.Token], AST)

parseTopLevel :: [T.Token] -> AST -> Return
parseTopLevel = parseBitOr

applyInner :: [(T.Token, (AST -> AST -> AST))] ->
            [(T.Token, (AST -> AST -> AST))] ->
            ([T.Token] -> AST -> Return) ->
            [T.Token] -> AST -> Return
applyInner _ _ _ [] ast = Right ([], ast)
applyInner _ [] nextFn tokens n1 = nextFn tokens n1
applyInner matches ((token, kind):rest) nextFn (o:tokens) n1 = do
  if o == token then do
    (tokens2, tmp) <- parseIdent tokens
    (tokens3, n2)  <- nextFn tokens2 tmp
    let ast = kind n1 n2
    case tokens3 of
      [] -> Right (tokens3, kind n1 n2)
      (token:_) -> do
        if any ((== token) . fst) matches
          then apply matches nextFn tokens3 ast
          else Right (tokens3, ast)
  else
    applyInner matches rest nextFn (o:tokens) n1

apply :: [(T.Token, (AST -> AST -> AST))] ->
            ([T.Token] -> AST -> Return) ->
            [T.Token] -> AST -> Return
apply matches nextFn tokens n1 = applyInner matches matches nextFn tokens n1

--------------------
-- Actual parsing --
--------------------

parseBitOr = apply [(T.BitOr, BitOr), (T.BitXor, BitXor)] parseBitAnd
parseBitAnd = apply [(T.BitAnd, BitAnd)] parseBitShift
parseBitShift = apply [(T.BitShiftL, BitShiftL), (T.BitShiftR, BitShiftR)] parsePlus

parsePlus = apply [(T.Plus, Add), (T.Minus, Sub)] parseMult
parseMult = apply [(T.Mult, Mult), (T.Div, Div), (T.Rem, Rem)] parsePow
parsePow  = apply [(T.Pow, Pow)] parseFac

parseFac :: [T.Token] -> AST -> Return
parseFac (T.Factorial:tokens) n = parseImplicitMult tokens (Factorial n)
parseFac tokens ast = parseImplicitMult tokens ast

parseImplicitMult :: [T.Token] -> AST -> Return
parseImplicitMult (T.GroupOpen:tokens) n = do
  (tokens2, group) <- parseIdent (T.GroupOpen:tokens)
  Right (tokens2, Mult n group)
parseImplicitMult tokens ast = parseVar tokens ast

parseVar :: [T.Token] -> AST -> Return
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
parseIdent (T.Number n:T.GroupOpen:tokens) = do
  (tokens2, num) <- parseIdent (T.Number n:tokens)
  (tokens3, group) <- parseIdent (T.GroupOpen:tokens)
  Right (tokens3, Mult num group)
parseIdent (T.Number n:tokens) = Right (tokens, Number n)
parseIdent (T.Text var:tokens) = Right (tokens, VarGet var)
parseIdent (T.Fn:T.Text var:tokens) = Right (tokens, FnCall var [])
parseIdent (T.GroupOpen:tokens) = do
  (tokens2, tmp)  <- parseIdent tokens
  (tokens3, ast2) <- parseInner tokens2 tmp False
  case tokens3 of
    [] -> Left "expected ), got EOF"
    (T.GroupClose:tokens4) -> Right (tokens4, ast2)
    (t:_) -> Left $ "expected ), got " ++ show t

parseIdent (t:_) = Left $ "expected number, got " ++ show t
parseIdent [] = Left "expected number, got EOF"
