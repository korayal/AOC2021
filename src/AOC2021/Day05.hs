module AOC2021.Day05 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Data.Attoparsec.Text
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
data Point = Point
  { pX :: Int,
    pY :: Int
  }
  deriving (Eq, Show)

data Edge = Edge
  { eFrom :: Point,
    eTo :: Point
  }
  deriving (Eq, Show)

isLine :: Edge -> Bool
isLine (Edge (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

printEdge :: Edge -> Text
printEdge (Edge (Point x1 y1) (Point x2 y2)) = T.pack $ concat [show x1, ",", show y1, " -> ", show x2, ",", show y2]

pointP :: Parser Point
pointP = do
  x <- decimal
  _ <- char ','
  y <- decimal
  pure $ Point x y

edgeP :: Parser Edge
edgeP = do
  p1 <- pointP
  _ <- string " -> "
  p2 <- pointP
  pure $ Edge p1 p2

t :: Parser [Edge]
t = do
  edgeP `sepBy` endOfLine

affectedCells :: Edge -> [(Int, Int)]
affectedCells (Edge (Point x1 y1) (Point x2 y2)) =
  if x1 == x2 || y1 == y2
    then [(x, y) | x <- [minW .. maxW], y <- [minH .. maxH]]
    else zip (range x1 x2) (range y1 y2)
  where
    range a b = [a, a + signum (b - a) .. b]
    minW = min x1 x2
    maxW = max x1 x2
    minH = min y1 y2
    maxH = max y1 y2

--------------------------------------------------------------------------------
diagram1 :: [Edge] -> HM.HashMap Int (HM.HashMap Int Int)
diagram1 = foldl' updateMap HM.empty
  where
    updateMap agg edge = foldl' updateValue agg (affectedCells edge)

    updateValue :: HM.HashMap Int (HM.HashMap Int Int) -> (Int, Int) -> HM.HashMap Int (HM.HashMap Int Int)
    updateValue aggMap (x, y) =
      case HM.lookup x aggMap of
        Nothing -> HM.insert x (HM.singleton y 1) aggMap
        Just yMap ->
          let newYMap = HM.alter (maybe (Just 1) (Just . (+ 1))) y yMap
           in HM.insert x newYMap aggMap

answer1Pure :: [Edge] -> Int
answer1Pure edges =
  length
    . filter (> 1)
    . concatMap HM.elems
    . HM.elems
    $ diagram1 (filter isLine edges)

--------------------------------------------------------------------------------
answer2Pure :: [Edge] -> Int
answer2Pure edges =
  length
    . filter (> 1)
    . concatMap HM.elems
    . HM.elems
    $ diagram1 edges

--------------------------------------------------------------------------------
readInput :: IO [Edge]
readInput = T.readFile "data/day05.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 05"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer2Pure]
