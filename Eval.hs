module Eval where
  import Data.Decimal
  import Parse

  eval :: AST -> Decimal
  eval (Add n1 n2)  = (eval n1) + (eval n2)
  eval (Sub n1 n2)  = (eval n1) - (eval n2)
  eval (Mult n1 n2) = (eval n1) * (eval n2)
  eval (Div n1 n2)  = (eval n1) / (eval n2)
  eval (Number n)   = n
