{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-unused-imports #-}

module AOC2021.Day13 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import Control.Lens
import Data.Attoparsec.Text hiding (take)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified GHC.Show as GS

--------------------------------------------------------------------------------
data Cell = C_Sharp | C_Dot deriving stock (Eq)

instance Show Cell where
  show = \case
    C_Sharp -> "#"
    C_Dot -> "·"

newtype Paper = Paper
  { _unPaper :: V.Vector (V.Vector Cell)
  }
  deriving newtype (Eq)

transposePaper :: Paper -> Paper
transposePaper (Paper p) = Paper . V.fromList . fmap V.fromList . transpose . toList $ fmap toList p

instance Show Paper where
  show (Paper v) = T.unpack . unlines . fmap showRow $ V.toList v
    where
      showRow :: V.Vector Cell -> Text
      showRow = T.intercalate " " . fmap (T.pack . show) . V.toList

data Instruction
  = I_X Int
  | I_Y Int
  deriving stock (Eq)

instance Show Instruction where
  show = \case
    I_X x -> "x=" <> show x
    I_Y y -> "y=" <> show y

makeLenses ''Paper

markP :: Parser (Int, Int)
markP = do
  x <- decimal
  _ <- char ','
  y <- decimal
  pure (x, y)

instructionP :: Parser Instruction
instructionP = do
  _ <- string "fold along "
  (I_X <$> (string "x=" >> decimal))
    <|> I_Y <$> (string "y=" >> decimal)

t :: Parser (Paper, NonEmpty Instruction)
t = do
  marks <- markP `sepBy` endOfLine
  skipSpace
  instructions <- instructionP `sepBy` endOfLine

  let (xSize :: Int) = L.maximum (fst <$> marks) + 1
      (ySize :: Int) = L.maximum (snd <$> marks) + 1
      xV = V.fromList (replicate xSize C_Dot)
      v = foldl' (updateCell (const C_Sharp)) (Paper $ V.fromList (replicate ySize xV)) marks

  pure (v, fromMaybe (error "instructions can't be empty") $ nonEmpty instructions)

updateCell :: (Cell -> Cell) -> Paper -> (Int, Int) -> Paper
updateCell f (Paper mNew) (x, y) = Paper $ mNew & ix y . ix x %~ f

--------------------------------------------------------------------------------
foldPaper :: Paper -> Instruction -> Paper
foldPaper pp@(Paper p) = \case
  I_X x -> transposePaper $ foldPaper (transposePaper pp) (I_Y x)
  I_Y y -> slicePaper y $ foldl' (mergeRows y) pp $ (V.zip (V.fromList [1, 2 .. yMax]) (bottomSlice y) :: V.Vector (Int, V.Vector Cell))
  where
    yMax = length p - 1
    bottomSlice y = V.slice (y + 1) (yMax - y) p
    slicePaper y (Paper ps) = Paper $ V.slice 0 y ps

    mergeRows :: Int -> Paper -> (Int, V.Vector Cell) -> Paper
    mergeRows y uP (i, row) = uP & unPaper . ix (y - i) %~ V.zipWith mergeRow row

    mergeRow C_Sharp _ = C_Sharp
    mergeRow _ C_Sharp = C_Sharp
    mergeRow a C_Dot = a

answer1Pure :: (Paper, NonEmpty Instruction) -> Int
answer1Pure (p, i) = sum . fmap (length . filter (== C_Sharp) . toList) . _unPaper $ foldPaper p (head i)

--------------------------------------------------------------------------------
answer2Pure :: (Paper, NonEmpty Instruction) -> Int
answer2Pure (p, i) = sum . fmap (length . filter (== C_Sharp) . toList) . _unPaper $ foldl' foldPaper p i

--------------------------------------------------------------------------------
readInput :: IO (Paper, NonEmpty Instruction)
readInput = T.readFile "data/day13.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 13"
  input <- readInput

  let (p, ins) = input

  print $ foldl' foldPaper p ins

  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
