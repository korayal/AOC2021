module AOC2021.Day01 where

import Data.List (zipWith3)
import Relude

import qualified Data.Text as T
import qualified Data.Text.IO as T

answer01 :: [Integer] -> Int
answer01 input = count - 1
  where
    count = length $ filter ((== "increased") . snd) zipped
    zipped = zipWith comp input (0:input)
    comp x y = case compare x y of
      LT -> (y, "decreased" :: String)
      EQ -> (y, "unchanged")
      GT -> (y, "increased")

answer02 :: [Integer] -> Int
answer02 input = answer01 sumsOfThrees
  where
    sumsOfThrees = case input of
      (x:y:z:zs) -> zipWith3 (\x y z -> x + y + z) input (y:z:zs) (z:zs)
      _ -> error  "invalid set"

readInputDay1 :: IO [Integer]
readInputDay1 = T.readFile "/tmp/day1a.txt" <&> mapMaybe (readMaybe . T.unpack) . lines

sampleInput :: [Integer]
sampleInput =
      [ 199 :: Integer,
        200,
        208,
        210,
        200,
        207,
        240,
        269,
        260,
        263
      ]
