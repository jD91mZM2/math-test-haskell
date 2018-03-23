import System.Console.Readline

import Eval
import Parse
import Tokenize

-- Because a CPP preprocessor is too much work
isDebug = True

mainLoop :: IO ()
mainLoop = do
  line <- readline "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line   -> do
      addHistory line
      case tokenize line [] of
        Left err -> putStrLn $ "tokenize error: " ++ err
        Right tokens -> do
          if isDebug
            then putStrLn $ "Debug: " ++ show tokens
            else do return ()
          case parse tokens of
            Left err -> putStrLn $ "parse error: " ++ err
            Right ast -> do
              if isDebug
                then putStrLn $ "Debug: " ++ show ast
                else do return ()
              print $ eval ast
      mainLoop

main = mainLoop
