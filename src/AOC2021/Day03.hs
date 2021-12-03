module AOC2021.Day03 where

--------------------------------------------------------------------------------

import Data.MonoTraversable (headMay)
import Data.Sequences (tailMay)
import Text.Printf
import Numeric (showIntAtBase)
import AOC2021.Prelude
import qualified Data.Text as T
import Data.Bits
import Data.Char (digitToInt, intToDigit)

--------------------------------------------------------------------------------
inputDataText :: String -> Maybe String
inputDataText = Just

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

favorHigher :: Int -> Char
favorHigher x =
  case compare x 0 of
    GT -> '1'
    EQ -> '1'
    LT -> '0'

favorLower :: Int -> Char
favorLower x =
  case compare x 0 of
    GT -> '1'
    EQ -> '0'
    LT -> '0'

--------------------------------------------------------------------------------
calcRate :: Char -> [String] -> [Int]
calcRate favChar =
  foldl' calcBits (repeat 0)
  . fmap (map incrementScore)
  where
    calcBits a b = zipWith (+) (a :: [Int]) (b :: [Int])
    incrementScore c = if c == favChar then 1 else -1

answer1Pure :: [String] -> Int
answer1Pure i = case headMay i of
  Nothing -> error "wat?"
  Just t -> let gr = toDec (map favorHigher (calcRate '1' i))
                mask = (2 ^ length t) - 1
            in gr * (mask .&. complement gr)

--------------------------------------------------------------------------------
calcGasRating :: Char -> (Int -> Char) -> [String] -> String
calcGasRating favChar favorFunc i = filterVals
  where
    hop = map favorFunc (calcRate favChar i)
    filterVals = case uncons hop of
      Nothing -> error "wat1"
      Just (x, xs) ->
        let filtered = mapMaybe tailMay $ filter ((==) (Just x) . headMay) i
        in case filtered of
          [] -> error "wat2"
          [y] -> x : y
          _ -> x : calcGasRating favChar favorFunc filtered

answer2Pure :: [String] -> Int
answer2Pure input = case headMay input of
  Nothing -> error "wat?"
  Just t -> let o2 = toDec (calcGasRating '1' favorHigher input)
                co2 = toDec (calcGasRating '0' favorLower input)
            in o2 * co2

--------------------------------------------------------------------------------
readInput :: IO [String]
readInput = readLineInput "data/day3.txt" (inputDataText . T.unpack)

result :: IO ()
result = do
  putStrLn "DAY 03"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
