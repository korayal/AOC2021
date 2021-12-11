module AOC2021.Day09 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Control.Lens
import Data.Attoparsec.Text hiding (take)
import Data.Char (digitToInt)
import Data.List (intersect)
import qualified Data.Set as Set
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
traceShow' :: Show a => a -> b -> b
traceShow' a b = b

--------------------------------------------------------------------------------
data Pos = Pos {pX :: Int, pY :: Int}

t :: Parser [[Int]]
t = do
  nums <- many1 digit `sepBy` char '\n'
  pure $ (fmap . fmap) digitToInt nums

--------------------------------------------------------------------------------
iterateSmallest :: (Int, [Int]) -> [(Int, Int, Int)]
iterateSmallest (i, nums) = fmap cleanup . foldl' go [] $ zip [0, 1 ..] nums
  where
    cleanup (a, b, _, _) = (i, a, b)
    go [] (iv, v) = [(iv, v, True, v)]
    go agg@((ia, a, isSmallest, highest) : xs) (iv, v) =
      if
          | v < a && isSmallest && highest /= a -> (iv, v, True, v) : agg
          | v < a && isSmallest -> (iv, v, True, v) : xs
          | v < a -> (iv, v, True, a) : agg
          | v < highest -> (iv, v, True, v) : agg
          | otherwise -> (ia, a, isSmallest, v) : xs

flipXY :: (Int, Int, Int) -> (Int, Int, Int)
flipXY (a, b, c) = (b, a, c)

answer1Pure :: [[Int]] -> Int
answer1Pure input =
  let is = concat (iterateSmallest <$> zip [0, 1 ..] input)
      ist = flipXY <$> concat (iterateSmallest <$> zip [0, 1 ..] (transpose input))
   in sum . fmap (+ 1) $ (is `intersect` ist) ^.. folded . _3

--------------------------------------------------------------------------------

iterateBasins :: (Int, [Int]) -> [[(Int, Int)]]
iterateBasins (i, nums) = (fmap . fmap) (i,) . fst $ foldl' go ([], 0) $ zip [0, 1 ..] nums
  where
    go ([], _) (iv, v) = if v == 9 then ([], v) else ([[iv]], v)
    go (agg@(a : as), prev) (iv, v) =
      if
          | v /= 9 && prev == 9 -> ([iv] : agg, v)
          | v /= 9 -> ((iv : a) : as, v)
          | otherwise -> (agg, v)

neighbors :: Int -> Int -> (Int, Int) -> Set.Set (Int, Int)
neighbors xBound yBound (xOld, yOld) =
  Set.fromList
    [ (x, y)
      | (x, y) <-
          map
            (bimap (+ xOld) (+ yOld))
            [(0, -1), (0, 1), (-1, 0), (1, 0)],
        x >= 0,
        y >= 0,
        x < xBound,
        y < yBound
    ]

matrixSize :: [[Int]] -> (Int, Int)
matrixSize m = fromMaybe (error "can't even calculate a matrix size") $ do
  let yLen = length m
  xLen <- length <$> headMay m
  pure (xLen, yLen)

readIndex :: [[Int]] -> (Int, Int) -> Int
readIndex m (x, y) = fromMaybe (error "trying to read the wrong index") $ do
  row <- m !!? y
  row !!? x

expandNeighbors :: [[Int]] -> (Int, Int) -> Set.Set (Int, Int)
expandNeighbors m initPos = fst $ go (Set.singleton initPos) Set.empty initPos
  where
    (xBound, yBound) = matrixSize m
    go agg targets targetPos =
      let (aggM, newTargetSet) = foldl' (expand m) (agg, targets) (neighbors xBound yBound targetPos)
          (nextTargetPos, newTargets) = Set.splitAt 1 (Set.filter ((== 0) . readIndex m) newTargetSet)
       in case headMay nextTargetPos of
            Nothing -> (aggM, Set.empty)
            Just newTargetPos -> go (Set.insert newTargetPos aggM) newTargets newTargetPos

expand ::
  [[Int]] ->
  (Set (Int, Int), Set (Int, Int)) ->
  (Int, Int) ->
  (Set (Int, Int), Set (Int, Int))
expand m (aggM, targets) targetPos =
  let (xBound, yBound) = matrixSize m
      indexVal = readIndex m targetPos
   in if
          | indexVal == 0 && not (Set.member targetPos aggM) ->
            let newAgg = Set.insert targetPos aggM
                nbs = neighbors xBound yBound targetPos
                newTargets = (targets `Set.union` nbs) `Set.difference` newAgg
             in (newAgg, newTargets)
          | indexVal == 0 ->
            let newAgg = Set.insert targetPos aggM
                newTargets = targets `Set.difference` newAgg
             in (newAgg, newTargets)
          | otherwise ->
            let newTargets = Set.delete targetPos targets
             in (aggM, newTargets)

answer2Pure :: [[Int]] -> Int
answer2Pure input =
  let is = concat (iterateSmallest <$> zip [0, 1 ..] input)
      ist = flipXY <$> concat (iterateSmallest <$> zip [0, 1 ..] (transpose input))
      newMap = input & traverse . traverse . filtered (/= 9) .~ 0
      deeps = is `intersect` ist
   in product . take 3 . sortOn Down . fmap length $ expandNeighbors newMap . (\(x, y, _) -> (y, x)) <$> deeps

--------------------------------------------------------------------------------
readInput :: IO [[Int]]
readInput = T.readFile "data/day09.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 09"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
