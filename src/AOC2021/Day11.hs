module AOC2021.Day11 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Control.Lens
import Data.Attoparsec.Text
import Data.Char (digitToInt)
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified GHC.Show as GS

--------------------------------------------------------------------------------

newtype Board = Board
  { _unBoard :: V.Vector (V.Vector Int)
  }
  deriving newtype (Eq)

makeLenses ''Board

instance Show Board where
  show (Board v) = LT.unpack $ LT.unlines . V.toList $ fmap show v

bingoLineP :: Parser (V.Vector Int)
bingoLineP = do
  l <- fmap digitToInt <$> many1 digit
  pure $ V.fromList l

t :: Parser Board
t = do
  linePs <- bingoLineP `sepBy` endOfLine
  endOfLine
  pure (Board $ V.fromList linePs)

--------------------------------------------------------------------------------

-- | indexes start from 1
neighbors :: Board -> (Int, Int) -> Set.Set (Int, Int)
neighbors (Board m) (xOld, yOld) =
  Set.fromList
    [ (x, y)
      | (x, y) <-
          [ bimap (+ xOld) (+ yOld) (a, b)
            | a <- [-1, 0, 1],
              b <- [-1, 0, 1],
              (a, b) /= (0, 0)
          ],
        x >= 0,
        y >= 0,
        x < xBound,
        y < yBound
    ]
  where
    xBound = V.length (m V.! 0)
    yBound = V.length m

getCoords :: (Int -> Bool) -> Board -> Set.Set (Int, Int)
getCoords f = foldl' go Set.empty . zip [0, 1 ..] . V.toList . _unBoard
  where
    go agg (y, row) = agg <> Set.fromList (fmap ((,y) . fst) (filter (f . snd) $ zip [0, 1 ..] (V.toList row)))

updateCell :: (Int -> Int) -> Board -> (Int, Int) -> Board
updateCell f (Board mNew) (x, y) = Board $ mNew & ix y . ix x %~ f

updateCells :: Foldable t => (Int -> Int) -> Board -> t (Int, Int) -> Board
updateCells f = foldl' (updateCell f)

mapNeighbors :: (Int -> Int) -> Board -> (Int, Int) -> Board
mapNeighbors f m = updateCells f m . neighbors m

-- | look for all 10s and iterate until the result is equal to previous version
bumpIterate :: Board -> (Board, Set.Set (Int, Int))
bumpIterate = go Set.empty
  where
    go ignoreTens m =
      let tens = getCoords (== 10) m
          foldedM = foldl' (mapNeighbors bump) m tens
          newM = updateCells (const 0) foldedM tens
          newTens = getCoords (> 9) newM `Set.difference` tens
       in if null newTens
            then (newM, ignoreTens `Set.union` tens)
            else go (ignoreTens `Set.union` tens) newM
    bump 0 = 0
    bump 10 = 10
    bump x = x + 1

bumpStep :: Board -> Board
bumpStep input = input & unBoard . traversed . traversed %~ (+ 1)

doRound :: Board -> (Board, Set.Set (Int, Int))
doRound = bumpIterate . bumpStep

iterateRoundWithTermination :: Int -> Board -> (Board, [Set (Int, Int)])
iterateRoundWithTermination n m = either id id $ foldl' go (Left (m, [])) [1 .. n]
  where
    go (Right (board, flashes)) _ = Right (board, flashes)
    go acc _ =
      acc <> case acc of
        Right _ -> error "we can't be here"
        Left (board, flashes) ->
          let (newBoard, newFlashes) = doRound board
              totalFlashes = newFlashes : flashes
           in if length newFlashes == 100
                then Right (newBoard, totalFlashes)
                else Left (newBoard, totalFlashes)

-- answer1Pure :: Board -> Int
answer1Pure :: Board -> Int
answer1Pure = sum . fmap length . snd . iterateRoundWithTermination 100

--------------------------------------------------------------------------------
answer2Pure :: Board -> Int
answer2Pure = length . snd . iterateRoundWithTermination 300

--------------------------------------------------------------------------------
readInput :: IO (Board)
readInput = T.readFile "data/day11.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 11"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
