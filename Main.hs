import System.Console.Readline

import Eval
import Parse
import Tokenize
import qualified Data.HashMap as M

-- Because a CPP preprocessor is too much work
isDebug = True

mainLoop :: M.Map String AST -> IO ()
mainLoop map = do
  line <- readline "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line   -> do
      addHistory line
      case tokenize line [] of
        Left err -> do
          putStrLn $ "tokenize error: " ++ err
          mainLoop map
        Right tokens -> do
          if isDebug
            then putStrLn $ "Debug: " ++ show tokens
            else do return ()
          case parse tokens of
            Left err -> do
              putStrLn $ "parse error: " ++ err
              mainLoop map
            Right ast -> do
              if isDebug
                then do
                  putStrLn $ "Debug: " ++ show ast
                  putStrLn $ "Variables: " ++ show (M.toList map)
                else do return ()
              let (map2, n) = eval map ast
              print n
              mainLoop map2

main = mainLoop M.empty
