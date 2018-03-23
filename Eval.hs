module Eval where

import Data.Bits
import Data.Decimal
import Parse

fac :: Decimal -> Decimal
fac 0 = 1
fac n = n * fac (n - 1)

eval :: AST -> Decimal
eval (Factorial n)  = fac (eval n)
eval (BitAnd n1 n2) = fromInteger $ (floor $ eval n1) .&. (floor $ eval n2)
eval (BitXor n1 n2) = fromInteger $ (floor $ eval n1) `xor` (floor $ eval n2)
eval (BitOr n1 n2)  = fromInteger $ (floor $ eval n1) .|. (floor $ eval n2)
eval (Mult n1 n2)   = (eval n1) * (eval n2)
eval (Div n1 n2)    = (eval n1) / (eval n2)
eval (Add n1 n2)    = (eval n1) + (eval n2)
eval (Sub n1 n2)    = (eval n1) - (eval n2)
eval (Number n)     = n
