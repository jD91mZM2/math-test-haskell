module Eval where
  import Data.Decimal
  import Parse

  fac :: Decimal -> Decimal
  fac 0 = 1
  fac n = n * fac (n - 1)

  eval :: AST -> Decimal
  eval (Add n1 n2)   = (eval n1) + (eval n2)
  eval (Sub n1 n2)   = (eval n1) - (eval n2)
  eval (Mult n1 n2)  = (eval n1) * (eval n2)
  eval (Div n1 n2)   = (eval n1) / (eval n2)
  eval (Factorial n) = fac (eval n)
  eval (Number n)    = n
