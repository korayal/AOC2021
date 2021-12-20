module AOC2021.Day12 where

--------------------------------------------------------------------------------

import AOC2021.Prelude hiding (show)
import Data.Attoparsec.Text
import qualified Data.DList as DList
import qualified Data.Graph.DGraph as DG
import Data.Graph.Types (IsEdge (destinationVertex))
import qualified Data.Graph.Types as GT
import qualified Data.GraphViz.Printing as GVP
import Data.MonoTraversable as MT
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import GHC.Show

--------------------------------------------------------------------------------
data V
  = V_Start
  | V_End
  | V_Small Text
  | V_Large Text
  deriving stock (Eq, Ord, Generic)

instance GVP.PrintDot V where
  unqtDot = GVP.unqtText . TL.pack . show

instance Show V where
  show = \case
    V_Start -> "start"
    V_Small t' -> T.unpack t'
    V_Large t' -> T.unpack t'
    V_End -> "end"

type instance MT.Element (DList.DList V) = V

instance Hashable V

instance MT.MonoFoldable (DList.DList V)

isVSmall :: V -> Bool
isVSmall (V_Small _) = True
isVSmall _ = False

toArc :: V -> V -> [GT.Arc V ()]
toArc V_Start v = [V_Start GT.--> v]
toArc v V_Start = [V_Start GT.--> v]
toArc V_End v = [v GT.--> V_End]
toArc v V_End = [v GT.--> V_End]
toArc v1 v2 = [v1 GT.--> v2, v2 GT.--> v1]

verticeP :: Parser V
verticeP =
  V_End <$ string "end"
    <|> V_Start <$ string "start"
    <|> V_Small <$> takeWhile1 (inClass ['a' .. 'z'])
    <|> V_Large <$> takeWhile1 (inClass ['A' .. 'Z'])

edgeP :: Parser [GT.Arc V ()]
edgeP = do
  v1 <- verticeP
  _ <- char '-'
  v2 <- verticeP
  pure $ toArc v1 v2

t :: Parser (DG.DGraph V ())
t = do
  l <- edgeP `sepBy` endOfLine
  pure $ DG.fromArcsList (concat l)

--------------------------------------------------------------------------------
type FilterCriteria = DList.DList V -> Maybe (NonEmpty (Bool, V)) -> Maybe (NonEmpty (Bool, V))

walkNodes :: FilterCriteria -> DG.DGraph V () -> [DList.DList V]
walkNodes f al = walkPath f al mempty (False, V_Start)

walkPath :: FilterCriteria -> DG.DGraph V () -> DList.DList V -> (Bool, V) -> [DList.DList V]
walkPath f g currentPath (b, v) =
  let neighborsMay = nonEmpty . fmap destinationVertex $ DG.outboundingArcs g v
      nextVerticesMay =
        case v of
          V_Start -> fmap (b,) <$> neighborsMay
          V_End -> Nothing
          _ -> f currentPath (fmap (b,) <$> neighborsMay)
   in case nextVerticesMay of
        Nothing
          | v == V_End -> [DList.snoc currentPath v]
          | otherwise -> []
        Just nextVertices -> concat $ walkPath f g (DList.snoc currentPath v) <$> toList nextVertices

--------------------------------------------------------------------------------
noDuplicateSmall :: FilterCriteria
noDuplicateSmall currentPath l =
  nonEmpty $ filter (\(_, x) -> not (sawBefore x)) $ maybe [] toList l
  where
    sawBefore x = isVSmall x && x `elem` currentPath

answer1Pure :: DG.DGraph V () -> Int
answer1Pure = length . walkNodes noDuplicateSmall

--------------------------------------------------------------------------------
singleDuplicateSmall :: FilterCriteria
singleDuplicateSmall currentPath l =
  nonEmpty
    . map (\(seenTwice, x) -> (seenTwice || (isVSmall x && sawBefore x), x))
    $ filter (\(seenTwice, x) -> not (sawBefore x && seenTwice)) $
      maybe [] toList l
  where
    sawBefore x = isVSmall x && x `elem` currentPath

answer2Pure :: DG.DGraph V () -> Int
answer2Pure = length . walkNodes singleDuplicateSmall

--------------------------------------------------------------------------------
readInput :: IO (DG.DGraph V ())
readInput = T.readFile "data/day12.txt" <&> either (error "failed parser") id . parseOnly t

result :: IO ()
result = do
  putStrLn "DAY 12"
  input <- readInput

  mapM_ print $ do
    map ($ input) [answer1Pure, answer2Pure]
