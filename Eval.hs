module Eval where

import Data.Bits
import Data.Decimal
import qualified Parse as P
import qualified Data.HashMap as M

type Map = M.Map String P.AST

fac :: Decimal -> Decimal
fac n
 | n <= 0 = 1
 | otherwise = n * fac (n - 1)

pow :: Decimal -> Decimal -> Decimal
pow base exp = powInner base (floor exp)

powInner :: Decimal -> Integer -> Decimal
powInner base 0 = 1
powInner base 1 = base
powInner base exp
  | exp < 0   = powInner (1 / base) (-exp)
  | odd exp   = base * powInner (base * base) (exp `shiftR` 1)
  | otherwise =        powInner (base * base) (exp `shiftR` 1)

evalAll :: Map -> [P.AST] -> [Decimal] -> Either [Char] (Map, [Decimal])
evalAll map [] decimals = Right (map, decimals)
evalAll map (n:asts) decimals = do
  (map2, e) <- eval map n
  evalAll map2 asts $ decimals ++ [e]

applyOne :: Map -> (Decimal -> Decimal) -> P.AST -> Either [Char] (Map, Decimal)
applyOne map fn n = do
  (map2, e) <- eval map n
  Right (map2, fn e)

applyOneInt :: Map -> (Integer -> Integer) -> P.AST -> Either [Char] (Map, Decimal)
applyOneInt map fn n = do
  (map2, e) <- eval map n
  Right (map2, fromIntegral $ fn (floor e))

apply :: Map -> (Decimal -> Decimal -> Decimal) -> P.AST -> P.AST -> Either [Char] (Map, Decimal)
apply map fn n1 n2 = do
  (map2, [e1, e2]) <- evalAll map [n1, n2] []
  Right (map2, fn e1 e2)

applyInt :: (Integral i) => Map -> (Integer -> i -> Integer) -> P.AST -> P.AST -> Either [Char] (Map, Decimal)
applyInt map fn n1 n2 = do
  (map2, [e1, e2]) <- evalAll map [n1, n2] []
  Right (map2, fromIntegral $ fn (floor e1) (floor e2))

eval :: Map -> P.AST -> Either [Char] (Map, Decimal)
eval map (P.Factorial n)  = applyOne map fac n
eval map (P.Mult n1 n2)   = apply map (*) n1 n2
eval map (P.Pow n1 n2)    = apply map pow n1 n2
eval map (P.Div n1 n2)    = apply map (/) n1 n2
eval map (P.Rem n1 n2)    = applyInt map rem n1 n2
eval map (P.Add n1 n2)    = apply map (+) n1 n2
eval map (P.Sub n1 n2)    = apply map (-) n1 n2
eval map (P.BitShiftL n1 n2) = applyInt map shiftL n1 n2
eval map (P.BitShiftR n1 n2) = applyInt map shiftR n1 n2
eval map (P.BitAnd n1 n2) = applyInt map (.&.) n1 n2
eval map (P.BitXor n1 n2) = applyInt map xor n1 n2
eval map (P.BitOr n1 n2)  = applyInt map (.|.) n1 n2

eval map (P.VarGet var) = do
  let val = M.lookup var map
  let map2 = M.delete var map -- Temporarily delete to prevent infinite loops
  (map3, e) <- eval map2 $ maybe (P.Number 0) id val
  case val of
    Just val2 -> Right (M.insert var val2 map3, e)
    Nothing -> Right (map3, e)
eval map (P.VarSet var val) = do
  (map2, e) <- eval map val
  Right (M.insert var val map2, e)
eval map (P.FnCall name argsAST) = do
  (map2, args) <- evalAll map argsAST []
  case name of
    "sum" -> Right (map2, sum args)
    _ -> Left $ "undefined function: " ++ name

eval map (P.Number n)   = Right (map, n)
eval map (P.Negative n) = applyOne map negate n
eval map (P.BitNot n) = applyOneInt map complement n
