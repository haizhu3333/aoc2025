module Main (main) where

import Control.Monad (guard)
import Data.List (tails1)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Geometry (V2(..))
import ReadInput (Parser, parseInput)

pointsP :: Parser [V2 Int]
pointsP = pointP `P.sepEndBy` P.newline
  where
    pointP = V2 <$> P.decimal <* P.char ',' <*> P.decimal

boxSize :: V2 Int -> V2 Int -> Int
boxSize (V2 x1 y1) (V2 x2 y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

largestBox :: [V2 Int] -> Int
largestBox points = maximum $ do
    p1 :| ps <- tails1 points
    boxSize p1 <$> ps

-- For part 2, assume input points wind clockwise.
-- Code to verify this for the inputs not included.

cornerOffsetCW :: V2 Int -> V2 Int -> V2 Int -> V2 Int
cornerOffsetCW prev curr next
    | dxIn == dyIn || dxOut == dyOut || dxIn == dxOut || dyIn == dyOut =
        error "Non-90 degree turn"
    | otherwise =
        let down = dyIn > 0 || dyOut > 0
            left = dxIn < 0 || dxOut < 0
        in  curr + V2 (fromEnum down) (fromEnum left)
  where
    V2 dxIn dyIn = signum (curr - prev)
    V2 dxOut dyOut = signum (next - curr)

cornersCW :: [V2 Int] -> [V2 Int]
cornersCW (p1 : p2 : ps) =
    zipWith3 cornerOffsetCW (p1 : p2 : ps) (p2 : ps ++ [p1]) (ps ++ [p1, p2])
cornersCW _ = error "At least 3 points required"

-- Segment 0 x y1 y2 represents the segment (x, y1) -- (x, y2)
-- Segment 1 y x1 x2 represents the segment (x1, y) -- (x2, y)
data Segment = Segment {
    compId :: !Int, compVal :: !Int, perpLB :: !Int, perpUB :: !Int }
    deriving Show

getSegment :: V2 Int -> V2 Int -> Segment
getSegment (V2 x1 y1) (V2 x2 y2)
    | x1 == x2 = Segment 0 x1 (min y1 y2) (max y1 y2)
    | y1 == y2 = Segment 1 y1 (min x1 x2) (max x1 x2)
    | otherwise = error "Points are not axis-aligned"

polygonSegments :: [V2 Int] -> [Segment]
polygonSegments ps = case cornersCW ps of
    [] -> error "corners are empty"
    c : cs -> zipWith getSegment (c : cs) (cs ++ [c])

boxSegments :: V2 Int -> V2 Int -> [Segment]
boxSegments (V2 x1 y1) (V2 x2 y2) =
    [ Segment 0 x1 ylo yhi
    , Segment 0 x2 ylo yhi
    , Segment 1 y1 xlo xhi
    , Segment 1 y2 xlo xhi
    ]
  where
    xlo = min x1 x2
    xhi = max x1 x2
    ylo = min y1 y2
    yhi = max y1 y2

-- We consider all coordinates of segment 1 to be offset by +1/2.
-- Thus some of the comparisons are <= and some are <.
crosses :: Segment -> Segment -> Bool
crosses (Segment c1 v1 lb1 ub1) (Segment c2 v2 lb2 ub2) =
    c1 /= c2 && lb2 <= v1 && v1 < ub2 && lb1 < v2 && v2 <= ub1

-- This searches all polygon segments (496 of them). Could maybe use a more
-- clever data structure, but it's fast enough.
boxCrosses :: V2 Int -> V2 Int -> [Segment] -> Bool
boxCrosses p1 p2 = or . liftA2 crosses (boxSegments p1 p2)

largestInsideBox :: [V2 Int] -> Int
largestInsideBox points = maximum $ do
    p1 :| ps <- tails1 points
    p2 <- ps
    guard $ not $ boxCrosses p1 p2 polygonS
    pure $ boxSize p1 p2
  where
    polygonS = polygonSegments points

main :: IO ()
main = do
    points <- parseInput 9 pointsP
    print $ largestBox points
    print $ largestInsideBox points
