module Parse where

import Data.Decimal
import qualified Tokenize as T

data AST =
    Number Decimal |
    Negative AST |
    BitNot AST  |
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

parse :: [(T.Span, T.Token)] -> Either (Maybe T.Span, [Char]) AST
parse tokens = do
  (tokens2, n) <- parseIdent tokens
  (tokens3, n2) <- parseInner tokens2 n True
  Right n2

parseInner :: [(T.Span, T.Token)] -> AST -> Bool -> Either (Maybe T.Span, [Char]) ([(T.Span, T.Token)], AST)
parseInner tokens ast toplevel = do
  (tokens2, ast2) <- parseTopLevel tokens ast
  case tokens2 of
    ((span, T.Number n):_) -> Left (Just span, "expected operator, found number " ++ show n)
    ((span, T.GroupClose):_) -> do
      if toplevel
        then Left (Just span, "expected operator, found )")
        else Right (tokens2, ast2)
    [] -> Right (tokens2, ast2)
    ((span, token):_) -> do
      Left (Just span, "trailing input was not parsed: " ++ show (map snd tokens2))

type Return = Either (Maybe T.Span, [Char]) ([(T.Span, T.Token)], AST)

parseTopLevel :: [(T.Span, T.Token)] -> AST -> Return
parseTopLevel = parseBitOr

applyInner :: [(T.Token, (AST -> AST -> AST))] ->
            [(T.Token, (AST -> AST -> AST))] ->
            ([(T.Span, T.Token)] -> AST -> Return) ->
            [(T.Span, T.Token)] -> AST -> Return
applyInner _ _ _ [] ast = Right ([], ast)
applyInner _ [] nextFn tokens n1 = nextFn tokens n1
applyInner matches ((token, kind):rest) nextFn tokens@((span, o):tokens2) n1 = do
  if o == token then do
    (tokens3, tmp) <- parseIdent tokens2
    (tokens4, n2)  <- nextFn tokens3 tmp
    let ast = kind n1 n2

    parseTopLevel tokens4 ast
  else
    applyInner matches rest nextFn tokens n1

apply :: [(T.Token, (AST -> AST -> AST))] ->
            ([(T.Span, T.Token)] -> AST -> Return) ->
            [(T.Span, T.Token)] -> AST -> Return
apply matches nextFn tokens n1 = applyInner matches matches nextFn tokens n1

--------------------
-- Actual parsing --
--------------------

parseBitOr = apply [(T.BitOr, BitOr), (T.BitXor, BitXor)] parseBitAnd
parseBitAnd = apply [(T.BitAnd, BitAnd)] parseBitShift
parseBitShift = apply [(T.BitShiftL, BitShiftL), (T.BitShiftR, BitShiftR)] parsePlus

parsePlus = apply [(T.Plus, Add), (T.Minus, Sub)] parseMult
parseMult = apply [(T.Mult, Mult), (T.Div, Div), (T.Rem, Rem)] parsePow

parsePow :: [(T.Span, T.Token)] -> AST -> Return
parsePow ((_, T.Pow):tokens) n = do
  (tokens2, n2) <- parseIdent tokens
  (tokens3, n3) <- parsePow tokens2 n2
  parsePow tokens3 (Pow n n3)
parsePow tokens ast = parseFac tokens ast

parseFac :: [(T.Span, T.Token)] -> AST -> Return
parseFac ((_, T.Factorial):tokens) n = parseFac tokens (Factorial n)
parseFac tokens ast = parseImplicitMult tokens ast

parseImplicitMult :: [(T.Span, T.Token)] -> AST -> Return
parseImplicitMult tokens@((_, T.GroupOpen):_) n = do
  (tokens2, group) <- parseIdent tokens
  Right (tokens2, Mult n group)
parseImplicitMult ((_, T.Ident ident):tokens) n = do
  Right (tokens, Mult n (VarGet ident))
parseImplicitMult tokens ast = parseVar tokens ast

parseVar :: [(T.Span, T.Token)] -> AST -> Return
parseVar ((_, T.Eq):tokens) (VarGet var) = do
  (tokens2, tmp) <- parseIdent tokens
  (tokens3, val) <- parseTopLevel tokens2 tmp
  Right (tokens3, VarSet var val)
parseVar ((span, T.Eq):tokens) ast = Left (Just span, "assignment expected name, got " ++ show ast)
parseVar tokens@((_, T.GroupClose):_) (FnCall name args) = Right (tokens, FnCall name args)
parseVar (t:tokens) (FnCall name args) = do
  (tokens2, arg) <- parseIdent (t:tokens)
  parseVar tokens2 $ FnCall name $ args ++ [arg]
parseVar tokens ast = Right (tokens, ast)

parseIdent :: [(T.Span, T.Token)] -> Return

parseIdent ((_, T.Minus):tokens) = do
  (tokens2, num) <- parseIdent tokens
  Right (tokens2, Negative num)
parseIdent ((_, T.BitNot):tokens) = do
  (tokens2, num) <- parseIdent tokens
  Right (tokens2, BitNot num)
parseIdent ((_, T.Number n):tokens) = Right (tokens, Number n)
parseIdent ((_, T.Ident var):tokens) = Right (tokens, VarGet var)
parseIdent ((_, T.Fn):(_, T.Ident var):tokens) = Right (tokens, FnCall var [])
parseIdent ((_, T.GroupOpen):tokens) = do
  (tokens2, tmp)  <- parseIdent tokens
  (tokens3, ast2) <- parseInner tokens2 tmp False
  case tokens3 of
    [] -> Left (Nothing, "expected ), got EOF")
    ((_, T.GroupClose):tokens4) -> Right (tokens4, ast2)
    ((span, t):_) -> Left (Just span, "expected ), got " ++ show t)

parseIdent ((span, t):_) = Left (Just span, "expected number, got " ++ show t)
parseIdent [] = Left (Nothing, "expected number, got EOF")
