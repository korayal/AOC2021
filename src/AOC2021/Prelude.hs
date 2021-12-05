module AOC2021.Prelude
  ( module X,
    readLineInput,
  )
where

--------------------------------------------------------------------------------

import Data.List as X (zipWith3)
import Data.MonoTraversable as X (headMay)
import qualified Data.Text.IO as T
import Relude as X

--------------------------------------------------------------------------------
readLineInput :: FilePath -> (Text -> Maybe a) -> IO [a]
readLineInput file f = T.readFile file <&> mapMaybe f . lines
