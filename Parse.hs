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
    n <- parseNum t
    out <- parsePlus tokens n
    let (tokens, ast) = out
    Right ast

  parsePlus :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
  parsePlus [] ast = Right ([], ast)
  parsePlus (t:[]) ast = do
    n <- parseNum t
    Right ([t], n)
  parsePlus (o:t:tokens) n1 = do
    if o == T.Plus || o == T.Minus then do
      tmp <- parseNum t
      (tokens2, n2) <- parseMult tokens tmp
      if o == T.Plus then
        parsePlus tokens2 $ Add n1 n2
      else
        parsePlus tokens2 $ Sub n1 n2
    else do
      parseMult (o:t:tokens) n1

  parseMult :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
  parseMult [] ast = Right ([], ast)
  parseMult (t:[]) ast = do
    n <- parseNum t
    Right ([t], n)
  parseMult (o:t:tokens) n1 = do
    if o == T.Mult || o == T.Div then do
      n2 <- parseNum t
      if o == T.Mult then
        parsePlus tokens $ Mult n1 n2
      else
        parsePlus tokens $ Div n1 n2
    else do
      Right (o:t:tokens, n1)

  parseNum :: T.Token -> Either [Char] AST
  parseNum (T.Number n) = Right $ Number n
  parseNum t = Left $ "expected number, got " ++ show t
