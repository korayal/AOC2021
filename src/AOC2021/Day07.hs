module AOC2021.Day07 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Data.Attoparsec.Text
import Data.Foldable (minimum)
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
newtype HP = HP {unHP :: Int}
  deriving newtype (Eq, Show, Num)

t :: Parser [HP]
t = fmap HP <$> decimal `sepBy` char ','

--------------------------------------------------------------------------------

diffList :: (Int -> Int) -> [HP] -> Int -> Int
diffList calcCost hps x = getSum . mconcat $ fmap (\hp -> Sum (calcCost (abs (x - unHP hp)))) hps

answer1Pure :: [HP] -> Int
answer1Pure hps =
  let posLen = length hps - 1
   in minimum (diffList id hps <$> [0 .. fromIntegral posLen])

--------------------------------------------------------------------------------
calcV2 :: Int -> Int
calcV2 distance = fromIntegral (distance * (distance + 1)) `div` 2

answer2Pure :: [HP] -> Int
answer2Pure hps =
  let posLen = length hps - 1
   in minimum (diffList calcV2 hps <$> [0 .. fromIntegral posLen])

--------------------------------------------------------------------------------
readInput :: IO [HP]
readInput = T.readFile "data/day07.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 07"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
