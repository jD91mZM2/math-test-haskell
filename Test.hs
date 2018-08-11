import Data.Char
import Data.Decimal
import Eval
import Parse
import System.Random
import Tokenize
import qualified Data.HashMap as M

calc :: [Char] -> Either [Char] Decimal
calc input = do
  tokens <- tokenize input
  ast <- parse tokens
  (_, result) <- eval M.empty ast
  Right result

test :: [Char] -> [Char] -> Decimal -> IO ()
test name input output = do
  putStr $ "testing " ++ name ++ "... "
  case calc input of
    Left err -> putStrLn $ "error: " ++ show err
    Right gotten -> do
      if gotten == output
        then putStrLn $ "passed"
        else putStrLn $ "failed. gotten: " ++ show gotten ++ ", expected: " ++ show output

rand :: IO Int
rand = getStdRandom random

iRem :: (Decimal -> Decimal -> Decimal)
iRem x y = fromIntegral $ (floor x) `rem` (floor y)

randOperator :: IO (Char, Decimal -> Decimal -> Decimal)
randOperator = do
  n <- getStdRandom $ randomR ((0, 1) :: (Int, Int))
  return $ case n of
    0 -> ('+', (+))
    1 -> ('-', (-))
    -- 2 -> ('*', (*))
    -- 3 -> ('/', (/))
    -- 4 -> ('%', iRem)

testRandom :: [Char] -> Decimal -> Int -> IO ()
testRandom input expected depth = do
  case depth of
    0 -> do
      n <- rand
      testRandom (input ++ show n) (fromIntegral n) (depth + 1)
    5 -> do
      case calc input of
        Left err -> putStrLn $ "error: " ++ show err
        Right gotten -> do
          if gotten == expected
            then putStr "."
            else putStrLn $ "random test \"" ++ input ++ "\" " ++
                              "failed. gotten: " ++ show gotten ++ ", expected: " ++ show expected
    _ -> do
      (o, f) <- randOperator
      n <- rand
      testRandom (input ++ [o] ++ show n) (f expected (fromIntegral n)) (depth + 1)

testRandoms :: Int -> IO ()
testRandoms n = do
  if n == 0
    then return ()
    else do
      testRandom "" 0 0
      if n `rem` 50 == 0
        then putStrLn ""
        else return ()
      testRandoms (n-1)

main :: IO ()
main = do
  test "simple"   "1 + 1"     2
  test "multiple" "1 + 2 + 3" 6
  test "order"    "1 + 2 * 3" 7
  test "fac"      "5!"        120
  test "zero fac" "0!"        1
  test "pow"      "5 ** 5"    3125
  test "zero pow" "5 ** 0"    1
  test "negative pow" "5 ** -3" 0.008
  test "bit or"   "3 | 5"     7
  test "bit xor"  "3 ^ 5"     6
  test "bit and"  "3 & 7"     3
  test "bitshift left" "1 << 3" 8
  test "bitshift right" "64 >> 2" 16
  test "binary" "0b101 | 0b010" 7
  test "octal"  "0o644 | 0o020" 436
  test "hex"    "0xFF + 1"    256
  test "equal level" "100 - 50 * 50 / 4" (-525)
  test "implicit multiplication" "100(4)" 400
  test "implicit multiplication ordering" "100 + 100(4)" 500
  test "implicit multiplication factorial" "5!(4)" 480
  test "characters" "'B' + ('a' - 'A')" (fromIntegral $ ord 'b')

  putStr "testing randoms"
  testRandoms 99
  putStrLn ""
