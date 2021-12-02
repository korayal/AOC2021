module AOC2021.Day02 where

import Data.List (zipWith3)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Relude

data Position = Position
  { horizontal :: Integer,
    depth :: Integer,
    aim :: Integer
  }
  deriving (Show)

mulPosition :: Position -> Integer
mulPosition Position {..} = horizontal * depth

initialPosition :: Position
initialPosition = Position 0 0 0

data Command
  = Command_Forward Integer
  | Command_Down Integer
  | Command_Up Integer
  deriving (Show)

commandText :: String -> Maybe Command
commandText ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : t) = Command_Forward <$> readMaybe t
commandText ('d' : 'o' : 'w' : 'n' : ' ' : t) = Command_Down <$> readMaybe t
commandText ('u' : 'p' : ' ' : t) = Command_Up <$> readMaybe t
commandText _ = Nothing

moveSub :: Position -> Command -> Position
moveSub p@Position {..} command = case command of
  Command_Forward m -> p {horizontal = horizontal + m}
  Command_Down m -> p {depth = depth + m}
  Command_Up m -> p {depth = depth - m}

moveSub2 :: Position -> Command -> Position
moveSub2 p@Position {..} command = case command of
  Command_Forward m -> p {horizontal = horizontal + m, depth = depth + aim * m}
  Command_Down m -> p {aim = aim + m}
  Command_Up m -> p {aim = aim - m}

answer21 :: [Command] -> Integer
answer21 = mulPosition . foldl' moveSub initialPosition

answer22 :: [Command] -> Integer
answer22 = mulPosition . foldl' moveSub2 initialPosition

readInputDay2 :: IO [Command]
readInputDay2 = T.readFile "/tmp/day2a.txt" <&> mapMaybe (commandText . T.unpack) . lines

result :: IO Integer
result = readInputDay2 <&> answer22
