module AOC2021.Day02 where

--------------------------------------------------------------------------------

import AOC2021.Prelude
import qualified Data.Text as T

--------------------------------------------------------------------------------
data Position = Position
  { horizontal :: Int,
    depth :: Int,
    aim :: Int
  }
  deriving (Show)

initialPosition :: Position
initialPosition = Position 0 0 0

--------------------------------------------------------------------------------
data Command
  = Command_Forward Int
  | Command_Down Int
  | Command_Up Int
  deriving (Show)

commandText :: String -> Maybe Command
commandText ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : t) = Command_Forward <$> readMaybe t
commandText ('d' : 'o' : 'w' : 'n' : ' ' : t) = Command_Down <$> readMaybe t
commandText ('u' : 'p' : ' ' : t) = Command_Up <$> readMaybe t
commandText _ = Nothing

--------------------------------------------------------------------------------
moveSub :: Position -> Command -> Position
moveSub p@Position {..} = \case
  Command_Forward m -> p {horizontal = horizontal + m}
  Command_Down m -> p {depth = depth + m}
  Command_Up m -> p {depth = depth - m}

moveSub2 :: Position -> Command -> Position
moveSub2 p@Position {..} = \case
  Command_Forward m -> p {horizontal = horizontal + m, depth = depth + aim * m}
  Command_Down m -> p {aim = aim + m}
  Command_Up m -> p {aim = aim - m}

mulPosition :: Position -> Int
mulPosition Position {..} = horizontal * depth

--------------------------------------------------------------------------------
answer1Pure :: [Command] -> Int
answer1Pure = mulPosition . foldl' moveSub initialPosition

--------------------------------------------------------------------------------
answer2Pure :: [Command] -> Int
answer2Pure = mulPosition . foldl' moveSub2 initialPosition

--------------------------------------------------------------------------------
readInput :: IO [Command]
readInput = readLineInput "data/day2.txt" (commandText . T.unpack)

result :: IO ()
result = do
  putStrLn "DAY 2"
  input <- readInput
  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
