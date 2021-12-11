module AOC2021.Day10 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
t :: Parser [String]
t = fmap T.unpack <$> takeWhile1 (inClass "<([{}])>") `sepBy` endOfLine

data CResult
  = CResult_Incomplete (NonEmpty Char)
  | CResult_Corrupted (NonEmpty Char)
  | CResult_Complete
  deriving (Show)

isIncomplete :: CResult -> Bool
isIncomplete (CResult_Incomplete _) = True
isIncomplete _ = False

incomplete :: CResult -> Maybe (NonEmpty Char)
incomplete (CResult_Incomplete ci) = Just ci
incomplete _ = Nothing

isCorrupted :: CResult -> Bool
isCorrupted (CResult_Corrupted _) = True
isCorrupted _ = False

isOpeningChar :: Char -> Bool
isOpeningChar c = c `elem` ("<([{" :: String)

isClosingChar :: Char -> Bool
isClosingChar c = c `elem` ("}])>" :: String)

closingChar :: Char -> Char
closingChar '<' = '>'
closingChar '(' = ')'
closingChar '[' = ']'
closingChar '{' = '}'
closingChar _ = error "unknow char"

--------------------------------------------------------------------------------
iterateChunks :: String -> CResult
iterateChunks = foldl' checkChunk CResult_Complete
  where
    checkChunk CResult_Complete c = if isOpeningChar c then CResult_Incomplete (one c) else CResult_Corrupted (one c)
    checkChunk cc@(CResult_Corrupted _) _ = cc
    checkChunk (CResult_Incomplete (x :| xs)) c =
      if
          | isClosingChar c && closingChar x == c -> case nonEmpty xs of
            Nothing -> CResult_Complete
            Just newXS -> CResult_Incomplete newXS
          | isClosingChar c -> CResult_Corrupted (c :| x : xs)
          | otherwise -> CResult_Incomplete (c :| x : xs)

scoreChar :: Char -> Int
scoreChar ')' = 3
scoreChar ']' = 57
scoreChar '}' = 1197
scoreChar '>' = 25137
scoreChar _ = 0

scoreCResult :: CResult -> Int
scoreCResult CResult_Complete = 0
scoreCResult (CResult_Incomplete _) = 0
scoreCResult (CResult_Corrupted (x :| xs)) = scoreChar x

answer1Pure :: [String] -> Int
answer1Pure =
  sum
    . fmap scoreCResult
    . filter (not . isIncomplete)
    . fmap iterateChunks

--------------------------------------------------------------------------------
closingScore :: Char -> Int
closingScore ')' = 1
closingScore ']' = 2
closingScore '}' = 3
closingScore '>' = 4
closingScore _ = 0

completeScore :: NonEmpty Char -> Int
completeScore = foldl' addClosingScore 0 . toList
  where
    addClosingScore agg x = closingScore (closingChar x) + (5 * agg)

median :: [Int] -> Int
median ls = fromMaybe (-1) (ls !!? mid)
  where
    mid = length ls `div` 2

answer2Pure :: [String] -> Int
answer2Pure =
  median
    . sort
    . fmap completeScore
    . catMaybes
    . fmap incomplete
    . fmap iterateChunks

--------------------------------------------------------------------------------
readInput :: IO [String]
readInput = T.readFile "data/day10.txt" <&> either (error "failed parser") id . parseOnly t

validChunks :: [String]
validChunks =
  [ "[]",
    "()",
    "([])",
    "{()()()}",
    "<([{}])>",
    "[<>({}){}[([])<>]]",
    "(((((((((())))))))))"
  ]

result :: IO ()
result = do
  putStrLn "DAY 10"
  input <- readInput

  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
