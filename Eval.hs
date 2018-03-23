module Eval where

import Data.Bits
import Data.Decimal
import qualified Parse as P
import qualified Data.HashMap as M

fac :: Decimal -> Decimal
fac n
 | n <= 0 = 1
 | otherwise = n * fac (n - 1)

apply :: M.Map String P.AST -> (Decimal -> Decimal -> Decimal) -> P.AST -> P.AST -> (M.Map String P.AST, Decimal)
apply map fn n1 n2 = do
  let (map2, e1) = eval map n1
  let (map3, e2) = eval map n2

  (map3, fn e1 e2)

applyInt :: (Integral i) => M.Map String P.AST -> (Integer -> i -> Integer) -> P.AST -> P.AST -> (M.Map String P.AST, Decimal)
applyInt map fn n1 n2 = do
  let (map2, e1) = eval map n1
  let (map3, e2) = eval map n2

  (map3, fromInteger $ fn (floor e1) (floor e2))

eval :: M.Map String P.AST -> P.AST -> (M.Map String P.AST, Decimal)
eval map (P.Factorial n)  = do
  let (map2, e) = eval map n
  (map, fac e)
eval map (P.Mult n1 n2)   = apply map (*) n1 n2
eval map (P.Div n1 n2)    = apply map (/) n1 n2
eval map (P.Add n1 n2)    = apply map (+) n1 n2
eval map (P.Sub n1 n2)    = apply map subtract n1 n2
eval map (P.BitShiftL n1 n2) = applyInt map shiftL n1 n2
eval map (P.BitShiftR n1 n2) = applyInt map shiftR n1 n2
eval map (P.BitAnd n1 n2) = applyInt map (.&.) n1 n2
eval map (P.BitXor n1 n2) = applyInt map xor n1 n2
eval map (P.BitOr n1 n2)  = applyInt map (.|.) n1 n2

eval map (P.VarGet var) = do
  let val = M.lookup var map
  let map2 = M.delete var map -- Temporarily delete to prevent infinite loops
  let (map3, e) = eval map2 $ maybe (P.Number 0) id val
  case val of
    Just val2 -> (M.insert var val2 map3, e)
    Nothing -> (map3, e)
eval map (P.VarSet var val) = do
  let (map2, e) = eval map val
  (M.insert var val map2, e)
eval map (P.Number n) = (map, n)
