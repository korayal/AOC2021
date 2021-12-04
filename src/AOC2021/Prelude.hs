module AOC2021.Prelude
  ( module X,
    readLineInput,
  ) where

--------------------------------------------------------------------------------

import Relude as X
import Data.MonoTraversable as X (headMay)
import Data.List as X (zipWith3)
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
readLineInput :: FilePath -> (Text -> Maybe a) -> IO [a]
readLineInput file f = T.readFile file <&> mapMaybe f . lines
