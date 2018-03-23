module Parse where
  import Data.Decimal
  import Data.Either
  import Data.Maybe
  import qualified Tokenize as T

  data AST =
      Number Decimal |
      Add AST AST |
      Sub AST AST |
      Mult AST AST |
      Div AST AST
    deriving Show

  parse :: [T.Token] -> Either [Char] AST
  parse (t:tokens) = do
    (tokens2, n) <- parseNum (t:tokens)
    (_, ast) <- parsePlus tokens2 n
    Right ast

  parsePlus :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
  parsePlus [] ast = Right ([], ast)
  parsePlus (t:[]) ast = do
    out <- parseNum [t]
    Right out
  parsePlus (o:t:tokens) n1 = do
    if o == T.Plus || o == T.Minus then do
      (tokens2, tmp) <- parseNum (t:tokens)
      (tokens3, n2) <- parseMult tokens2 tmp
      if o == T.Plus then
        parsePlus tokens3 $ Add n1 n2
      else
        parsePlus tokens3 $ Sub n1 n2
    else do
      parseMult (o:t:tokens) n1

  parseMult :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
  parseMult [] ast = Right ([], ast)
  parseMult (t:[]) ast = do
    (tokens, n) <- parseNum [t]
    Right (tokens, n)
  parseMult (o:t:tokens) n1 = do
    if o == T.Mult || o == T.Div then do
      (tokens2, n2) <- parseNum (t:tokens)
      if o == T.Mult then
        parsePlus tokens2 $ Mult n1 n2
      else
        parsePlus tokens2 $ Div n1 n2
    else do
      Right (o:t:tokens, n1)

  parseNum :: [T.Token] -> Either [Char] ([T.Token], AST)
  parseNum (T.Minus:T.Number n:tokens) = Right (tokens, Number $ -n)
  parseNum (T.Number n:tokens) = Right (tokens, Number n)
  parseNum (t:_) = Left $ "expected number, got " ++ show t
