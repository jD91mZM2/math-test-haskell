import System.Console.Readline
import qualified Data.HashMap as M

import Eval
import Parse
import Tokenize

-- Because a CPP preprocessor is too much work
isDebug = False
prompt = "> "

printSpan :: Span -> IO ()
printSpan (Span start end) =
  putStrLn $
    replicate (length prompt + start) ' ' ++
    case end of
      Just end -> replicate (end-start) '^'
      Nothing -> ['^']

mainLoop :: M.Map String AST -> IO ()
mainLoop map = do
  line <- readline "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line   -> do
      addHistory line

      case tokenize line of
        Left (span, err) -> do
          printSpan span
          putStrLn $ "tokenize error: " ++ err
          mainLoop map
        Right [] -> do
          putStrLn "\x1b[A|"
          mainLoop map
        Right tokens -> do
          if isDebug
            then putStrLn $ "Debug: " ++ show (Prelude.map snd tokens)
            else do return ()

          case parse tokens of
            Left (span, err) -> do
              case span of
                Just span -> printSpan span
                Nothing -> return ()
              putStrLn $ "parse error: " ++ err
              mainLoop map
            Right ast -> do
              if isDebug
                then do
                  putStrLn $ "Debug: " ++ show ast
                  putStrLn $ "Variables: " ++ show (M.toList map)
                else do return ()

              case eval map ast of
                Left err -> do
                  putStrLn $ "eval error: " ++ err
                  mainLoop map
                Right (map2, n) -> do
                  putStrLn $ "= " ++ show n
                  mainLoop map2

main :: IO ()
main = mainLoop M.empty
