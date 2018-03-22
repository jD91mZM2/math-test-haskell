import System.Console.Readline

import Eval
import Parse
import Tokenize

mainLoop :: IO ()
mainLoop = do
  line <- readline "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line   -> do
      addHistory line
      let tokens = tokenize line []
      putStrLn $ "Debug: " ++ show tokens
      case parse tokens of
        Left err -> putStrLn $ "error: " ++ err
        Right ast -> do
          putStrLn $ "Debug: " ++ show ast
          print $ eval ast
      mainLoop

main = do
  mainLoop
