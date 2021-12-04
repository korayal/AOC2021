module AOC2021.Day04 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Data.Attoparsec.Text
import Data.List (elemIndex)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Matrix as X

--------------------------------------------------------------------------------
data BingoCard = BingoCard
  { bingoCard :: X.Matrix Int
  , bingoResult :: X.Matrix Bool
  }
  deriving stock (Eq, Show)

resultToNegateMask :: X.Matrix Bool -> X.Matrix Int
resultToNegateMask = fmap (\b -> if b then 0 else 1)

showCard BingoCard{..} = do
  print bingoCard
  print bingoResult

data Inp = Inp
  { inpSeq :: [Int],
    inpCards :: [BingoCard]
  }
  deriving stock (Eq, Show)

showInput Inp{..} = do
  print inpSeq
  forM_ inpCards $ \card -> do
    showCard card
    putStrLn "-"

--------------------------------------------------------------------------------
drawSequence :: Parser [Int]
drawSequence = decimal `sepBy` char ','

bingoLineP :: Parser [Int]
bingoLineP = skipWhile (== ' ') >> many1 (decimal <* many (char ' '))

bingoCardP :: Parser BingoCard
bingoCardP = do
  lines <- bingoLineP `sepBy` endOfLine
  endOfLine
  let m = X.fromLists lines
      rm = False <$ m
  pure $ BingoCard m rm

t :: Parser Inp
t = do
  digits <- drawSequence
  skipSpace
  bingoCards <- bingoCardP `sepBy` endOfLine
  endOfInput
  pure $ Inp digits bingoCards

--------------------------------------------------------------------------------
x = X.fromLists $ [
  [True, True, False, True, False],
  [False, True, False, True, False],
  [True, True, False, True, True],
  [False, True, False, False, False]
  ]

isWinningCard :: X.Matrix Bool -> Bool
isWinningCard x = isJust $ rowsMay <|> colsMay
  where
    rowLists = X.toLists x
    rowsMay = elemIndex True $ fmap and rowLists
    colLists = X.toLists (X.transpose x)
    colsMay = elemIndex True $ fmap and colLists

isFinalCard :: X.Matrix Bool -> Bool
isFinalCard x = rowsMay
  where
    rowLists = X.toLists x
    rowsMay = and $ fmap and rowLists

processDraw  ::  [BingoCard] -> Int -> ([BingoCard], Maybe BingoCard)
processDraw bc x =
  -- we assume that there's only a single winning card, that feels wrong
  (updatedCards, headMay (filter (isWinningCard . bingoResult) updatedCards))
  where
    updatedCards = fmap updateCard bc
    updateCard bc@BingoCard{..} =
      bc{ bingoResult = X.mapPos (\(px, py) y -> (X.getElem px py bingoCard == x) || y) bingoResult }

answer1Pure :: Inp -> Int
answer1Pure Inp{..} = go inpCards inpSeq
  where
    go cards [] = error "no result"
    go cards (x:xs) = case processDraw cards x of
      (newCards, Nothing) -> go newCards xs
      (newCards, Just BingoCard{..}) -> sum (X.elementwise (*) (resultToNegateMask bingoResult) bingoCard) * x

--------------------------------------------------------------------------------
answer2Pure :: Inp -> Int
answer2Pure Inp{..} = go 0 inpCards inpSeq
  where
    go winner _ [] = error (show winner)
    go winner [] _ = winner
    go winner cards (x:xs) = case processDraw cards x of
      (newCards, Nothing) -> go winner newCards xs
      (newCards, Just winningCard) ->
        let newWinner = sum (X.elementwise (*) (resultToNegateMask (bingoResult winningCard)) (bingoCard winningCard)) * x
            -- this would mean the last result is expected to be a single card
            updatedCards = filter (not . isWinningCard . bingoResult) newCards
        in go newWinner updatedCards xs

--------------------------------------------------------------------------------
readInput :: IO Inp
readInput = T.readFile "data/day4.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 04"

  input <- readInput

  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
