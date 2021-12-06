{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-unused-imports #-}

module AOC2021.DayXXX where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
data InputData

t :: Parser [InputData]
t = undefined

--------------------------------------------------------------------------------
answer1Pure :: [InputData] -> Int
answer1Pure = undefined

--------------------------------------------------------------------------------
answer2Pure :: [InputData] -> Int
answer2Pure = undefined

--------------------------------------------------------------------------------
readInput :: IO [InputData]
readInput = T.readFile "data/dayXXXsample.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY XXX"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
