module AOC2021.Day01 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import qualified Data.Text as T

--------------------------------------------------------------------------------

data State
  = State_Decreased
  | State_Increased
  | State_Unchanged
  deriving stock (Eq)

--------------------------------------------------------------------------------
answer1Pure :: [Int] -> Int
answer1Pure input = count - 1
  where
    count = length $ filter ((== State_Increased) . snd) zipped
    zipped = zipWith comp input (0 : input)
    comp x y = case compare x y of
      LT -> (y, State_Decreased)
      EQ -> (y, State_Unchanged)
      GT -> (y, State_Increased)

--------------------------------------------------------------------------------
answer2Pure :: [Int] -> Int
answer2Pure input = answer1Pure sumsOfThrees
  where
    sumsOfThrees = case input of
      (_ : y : z : zs) -> zipWith3 (\a b c -> a + b + c) input (y : z : zs) (z : zs)
      _ -> error "invalid input"

--------------------------------------------------------------------------------
readInput :: IO [Int]
readInput = readLineInput "data/day01.txt" (readMaybe . T.unpack)

result :: IO ()
result = do
  putStrLn "DAY 1"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
