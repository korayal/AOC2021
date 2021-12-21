module AOC2021.Day14 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Control.Error
import Control.Lens
import Data.Attoparsec.Text
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
data InputData

pairP :: Parser ((Char, Char), Char)
pairP = do
  k1 <- anyChar
  k2 <- anyChar
  _ <- string " -> "
  v <- anyChar
  pure ((k1, k2), v)

t' :: Parser (HM.HashMap (Char, Char) Char, HM.HashMap (Char, Char) Int)
t' = do
  template <- T.unpack <$> takeWhile1 (inClass ['A' .. 'Z'])
  skipSpace
  pairs <- pairP `sepBy` endOfLine

  let templateTail = DList.tail $ DList.snoc (DList.fromList template) '*'
      updateTemplateMap hm (l, r) = hm & at (l, r) %~ Just . maybe 1 (+ 1)
      templateMap = foldl' updateTemplateMap HM.empty $ zip template templateTail

  pure (HM.fromList pairs, templateMap)

--------------------------------------------------------------------------------
iteratePairs :: HM.HashMap (Char, Char) Char -> HM.HashMap (Char, Char) Int -> HM.HashMap (Char, Char) Int
iteratePairs pm tm = foldl' checkMap tm $ HM.toList tm
  where
    checkMap :: HM.HashMap (Char, Char) Int -> ((Char, Char), Int) -> HM.HashMap (Char, Char) Int
    checkMap oldTM ((l, r), cnt) = case HM.lookup (l, r) pm of
      Nothing -> oldTM
      Just c ->
        oldTM & at (l, r) %~ dropIfZero . maybe 0 (\x -> x - cnt)
          & at (l, c) %~ Just . maybe cnt (+ cnt)
          & at (c, r) %~ Just . maybe cnt (+ cnt)

    dropIfZero x = if x == 0 then Nothing else Just x

aggregateCounts :: HM.HashMap (Char, Char) Int -> HM.HashMap Char (Int, Int)
aggregateCounts = foldl' updateCount HM.empty . HM.toList
  where
    updateCount m ((l, '*'), 1) =
      m & at l
        %~ ( \case
               Nothing -> Just (1, 0)
               Just (lc, rc) -> Just (lc + 1, rc)
           )
    updateCount m ((l, r), cnt) =
      m & at l
        %~ ( \case
               Nothing -> Just (cnt, 0)
               Just (lc, rc) -> Just (lc + cnt, rc)
           )
        & at r
        %~ ( \case
               Nothing -> Just (0, cnt)
               Just (lc, rc) -> Just (lc, rc + cnt)
           )

solve :: (HashMap (Char, Char) Char, HashMap (Char, Char) Int) -> Int -> Int
solve (pm, tm) its =
  let iteration = fromMaybe (error "haydar") $ iterate (iteratePairs pm) tm !!? its
      getActualCount (x, y) = (x + y + 1) `div` 2
      nums = HM.elems . fmap getActualCount $ aggregateCounts iteration
   in fromMaybe 0 (maximumMay nums) - fromMaybe 0 (minimumMay nums)

answer1Pure :: (HashMap (Char, Char) Char, HashMap (Char, Char) Int) -> Int
answer1Pure (pm, tm) = solve (pm, tm) 10

--------------------------------------------------------------------------------
answer2Pure :: (HashMap (Char, Char) Char, HashMap (Char, Char) Int) -> Int
answer2Pure (pm, tm) = solve (pm, tm) 40

--------------------------------------------------------------------------------
readInput :: IO (HashMap (Char, Char) Char, HashMap (Char, Char) Int)
readInput = T.readFile "data/day14.txt" <&> either (error "failed parser") id . parseOnly t'

result :: IO ()
result = do
  putStrLn "DAY 14"
  input <- readInput

  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
