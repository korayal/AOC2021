module AOC2021.Day08 where

--------------------------------------------------------------------------------

import AOC2021.Prelude hiding (takeWhile)
import Control.Lens
import Data.Attoparsec.Text
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------

lineP :: Parser ([Text], [Text])
lineP = do
  i <- takeWhile1 (inClass "abcdefg") `sepBy` char ' '
  _ <- string " | "
  o <- takeWhile1 (inClass "abcdefg") `sepBy` char ' '
  pure (i, o)

t :: Parser [([Text], [Text])]
t = lineP `sepBy` endOfLine

actualMap :: [(String, Int)]
actualMap =
  [ ("abcdefg", 8),
    ("cf", 1),
    ("acf", 7),
    ("bcdf", 4),
    ("abcdfg", 9),
    ("abdefg", 6),
    ("abcefg", 0),
    ("abdfg", 5),
    ("acdfg", 3),
    ("acdeg", 2)
  ]

solveChars :: [String] -> Maybe (HM.HashMap String Int)
solveChars strs = do
  let m = foldl' (\agg (k, v) -> HM.insertWith (<>) k v agg) HM.empty $ map (\x -> (length x, [x])) (fmap Set.fromList strs)

  seg1 <- m ^? ix 2 . to headMay . _Just
  seg8 <- m ^? ix 7 . to headMay . _Just
  seg7 <- m ^? ix 3 . to headMay . _Just
  seg4 <- m ^? ix 4 . to headMay . _Just

  sixes <- m ^? ix 6

  let (seg9May, sixes') = List.partition (\x -> (== 1) . length $ x `Set.difference` (seg7 <> seg4)) sixes
  seg9 <- headMay seg9May

  let (seg6May, seg0May) = List.partition (\x -> not $ seg1 `Set.isSubsetOf` x) sixes'
  seg6 <- headMay seg6May
  seg0 <- headMay seg0May

  fives <- m ^? ix 5

  let (seg5May, fives') = List.partition (\x -> Set.null $ x `Set.difference` seg6) fives
  seg5 <- headMay seg5May

  let (seg3May, seg2May) = List.partition (\x -> Set.null $ x `Set.difference` (seg5 <> seg1)) fives'
  seg3 <- headMay seg3May
  seg2 <- headMay seg2May

  pure $
    mempty
      & at (Set.toList seg0) ?~ 0
      & at (Set.toList seg1) ?~ 1
      & at (Set.toList seg2) ?~ 2
      & at (Set.toList seg3) ?~ 3
      & at (Set.toList seg4) ?~ 4
      & at (Set.toList seg5) ?~ 5
      & at (Set.toList seg6) ?~ 6
      & at (Set.toList seg7) ?~ 7
      & at (Set.toList seg8) ?~ 8
      & at (Set.toList seg9) ?~ 9

-- --------------------------------------------------------------------------------
answer1Pure :: [([Text], [Text])] -> Int
answer1Pure = foldl' getLength 0 . concat . fmap snd
  where
    getLength acc x =
      if T.length x `elem` [2, 3, 4, 7]
        then acc + 1
        else acc

--------------------------------------------------------------------------------
answer2Pure :: [([Text], [Text])] -> Int
answer2Pure = sum . fmap fixOutput
  where
    toNumber = sum . zipWith (*) [1000, 100, 10, 1]
    fixOutput (i, o) = case solveChars (fmap T.unpack i) of
      Nothing -> error "bad map"
      Just cmap -> toNumber $ catMaybes (o <&> \s -> (cmap ^? ix (sort $ T.unpack s)))

--------------------------------------------------------------------------------
readInput :: Parser a -> IO a
readInput pf = T.readFile "data/day08.txt" <&> either (error "failed parser") id . parseOnly pf

result :: IO ()
result = do
  putStrLn "DAY 08"
  input <- readInput t

  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
