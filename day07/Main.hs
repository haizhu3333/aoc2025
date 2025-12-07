module Main (main) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Data.Maybe (fromMaybe)
import ReadInput (Grid, readInputGrid)

findS :: Grid -> Ix2
findS = fromMaybe (error "No S found") . A.findIndex (== 'S')

fallDown :: Grid -> Map Ix2 Int -> (Int, Map Ix2 Int)
fallDown g pts = (M.size splits, M.unionsWith (+) [empties, splitsL, splitsR])
  where
    isInBounds p _ = A.isSafeIndex (A.size g) p
    hasSplitter p _ = A.indexM g p == Just '^'
    pts' = M.filterWithKey isInBounds $ M.mapKeys (+ (1 :. 0)) pts
    (splits, empties) = M.partitionWithKey hasSplitter pts'
    splitsL = M.mapKeys (+ (0 :. -1)) splits
    splitsR = M.mapKeys (+ (0 :. 1)) splits

countSplitsAndPaths :: Grid -> (Int, Int)
countSplitsAndPaths g = go 0 (M.singleton (findS g) 1)
  where
    go !acc pts =
        let (nSplit, pts') = fallDown g pts
            acc' = acc + nSplit
        in  if M.null pts'
            then (acc', sum pts)
            else go acc' pts'

main :: IO ()
main = do
    grid <- readInputGrid 7
    let (nSplits, nPaths) = countSplitsAndPaths grid
    print nSplits
    print nPaths
