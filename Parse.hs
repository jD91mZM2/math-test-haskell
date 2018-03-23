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
      Div AST AST |
      Factorial AST
    deriving Show

  parse :: [T.Token] -> Either [Char] AST
  parse (tokens) = do
    (tokens2, n) <- parseNum tokens
    (_, ast) <- parsePlus tokens2 n
    Right ast

  parsePlus :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
  parsePlus [] ast = Right ([], ast)
  parsePlus (o:tokens) n1 = do
    if o == T.Plus || o == T.Minus then do
      (tokens2, tmp) <- parseNum tokens
      (tokens3, n2) <- parseMult tokens2 tmp
      if o == T.Plus then
        parsePlus tokens3 $ Add n1 n2
      else
        parsePlus tokens3 $ Sub n1 n2
    else do
      parseMult (o:tokens) n1

  parseMult :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
  parseMult [] ast = Right ([], ast)
  parseMult (o:tokens) n1 = do
    if o == T.Mult || o == T.Div then do
      (tokens2, tmp) <- parseNum tokens
      (tokens3, n2) <- parseFac tokens2 tmp
      if o == T.Mult then
        parsePlus tokens3 $ Mult n1 n2
      else
        parsePlus tokens3 $ Div n1 n2
    else do
      parseFac (o:tokens) n1

  parseFac :: [T.Token] -> AST -> Either [Char] ([T.Token], AST)
  parseFac [] ast = Right ([], ast)
  parseFac (o:tokens) n1 = do
    if o == T.Factorial then do
      parsePlus tokens $ Factorial n1
    else do
      Right (o:tokens, n1)

  parseNum :: [T.Token] -> Either [Char] ([T.Token], AST)
  parseNum (T.Minus:T.Number n:tokens) = Right (tokens, Number $ -n)
  parseNum (T.Number n:tokens) = Right (tokens, Number n)
  parseNum (t:_) = Left $ "expected number, got " ++ show t
