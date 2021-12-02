{-# OPTIONS_GHC -fno-warn-deprecations #-}
module AOC2021.DayXXX where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import qualified Data.Text as T

--------------------------------------------------------------------------------
data InputData

inputDataText :: String -> Maybe InputData
inputDataText = undefined

--------------------------------------------------------------------------------
answer1Pure :: [InputData] -> Int
answer1Pure = undefined

--------------------------------------------------------------------------------
answer2Pure :: [InputData] -> Int
answer2Pure = undefined

--------------------------------------------------------------------------------
readInput :: IO [InputData]
readInput = readLineInput "data/dayXXX.txt" (inputDataText . T.unpack)

result :: IO ()
result = do
  putStrLn "DAY XXX"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
