module Main (main) where

import Data.Massiv.Array (Array, Ix2(..), U)
import qualified Data.Massiv.Array as A
import ReadInput (Grid, readInputGrid)

convertGrid :: Grid -> Array U Ix2 Int
convertGrid = A.compute . A.map f
  where
    f '.' = 0
    f '@' = 1
    f ch = error $ "Unexpected character " ++ show ch

accessibleRolls :: Array U Ix2 Int -> Array U Ix2 Int
accessibleRolls grid = A.compute $ A.zipWith (*) grid accessible
  where
    counts = A.applyStencil (A.Padding 1 1 (A.Fill 0)) (A.sumStencil $ A.Sz2 3 3) grid
    accessible = A.compute @U $ fmap (fromEnum . (<= 4)) counts

removeAccessible :: Array U Ix2 Int -> Array U Ix2 Int
removeAccessible grid
    | A.all (== 0) accessible = grid
    | otherwise = removeAccessible $ A.compute $ A.zipWith (-) grid accessible
  where
    accessible = accessibleRolls grid

main :: IO ()
main = do
    grid <- convertGrid <$> readInputGrid 4
    print $ A.sum $ accessibleRolls grid
    let removedAll = removeAccessible grid
    print $ A.sum grid - A.sum removedAll
