module AOC2021.Day06 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Control.Lens
import Data.Attoparsec.Text hiding (take)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------

newtype Countdown = Countdown Int
  deriving newtype (Eq, Show, Num, Hashable)

proceedLF :: HM.HashMap Countdown Int -> HM.HashMap Countdown Int
proceedLF hm =
  case HM.lookup 0 hm of
    Nothing -> decrementMap hm
    Just newBabies ->
      decrementMap hm
        & at 6 %~ Just . maybe newBabies (+ newBabies)
        & at 8 ?~ newBabies
  where
    decrementMap h =
      HM.mapKeys (\x -> x - 1) h
        & at (-1) .~ Nothing

t :: Parser (HM.HashMap Countdown Int)
t = do
  cnts <- fmap Countdown <$> decimal `sepBy` char ','
  pure $ foldl' go HM.empty cnts
  where
    go hm x = hm & at x %~ Just . maybe 1 (+ 1)

--------------------------------------------------------------------------------
answer1Pure :: HM.HashMap Countdown Int -> Int
answer1Pure lfs = maybe 0 (sum . HM.elems) (iterate proceedLF lfs !!? 80)

--------------------------------------------------------------------------------
answer2Pure :: HM.HashMap Countdown Int -> Int
answer2Pure lfs = maybe 0 (sum . HM.elems) (iterate proceedLF lfs !!? 256)

--------------------------------------------------------------------------------
readInput :: IO (HM.HashMap Countdown Int)
readInput = T.readFile "data/day06.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 06"
  input <- readInput

  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
