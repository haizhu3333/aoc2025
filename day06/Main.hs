module Main (main) where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Massiv.Array (Array, Ix1, Ix2(..), D)
import Data.Monoid (All(..))
import qualified Data.Massiv.Array as A
import ReadInput (Grid, readInputGrid)
import Data.Char (digitToInt, isDigit)

spaceColumns :: Grid -> [Ix1]
spaceColumns g = toList $ A.simapMaybe (\ix e -> ix <$ guard e) cols
  where
    cols = fmap getAll $ A.foldWithin A.Dim2 $ A.map (All . (== ' ')) g

splitSpaceColumns :: Grid -> [Array D Ix2 Char]
splitSpaceColumns g = do
    (x, y) <- zip (-1 : spaces) (spaces ++ [cols])
    pure $ A.extractFromTo' (0 :. (x+1)) (rows :. y) g
  where
    A.Sz (rows :. cols) = A.size g
    spaces = spaceColumns g

breakOperator :: Array D Ix2 Char -> (Array D Ix2 Char, Char)
breakOperator g = case A.splitAt' (A.Dim 2) (rows - 1) g of
    (g', opRow) -> (g', A.evaluate' opRow (0 :. 0))
  where
    A.Sz (rows :. _) = A.size g

rebuildNumbers :: Array D Ix2 Char -> Array D Ix1 Int
rebuildNumbers = A.foldlInner addChar 0
  where
    addChar n ' ' = n
    addChar n ch | isDigit ch = n * 10 + digitToInt ch
                 | otherwise = error $ "Invalid character " ++ show ch

calculate :: Array D Ix2 Char -> Char -> Int
calculate g op = applyOp op (rebuildNumbers g)
  where
    applyOp '+' = A.sum
    applyOp '*' = A.product
    applyOp ch = error $ "Invalid operator " ++ show ch

calculateAll :: (Array D Ix2 Char -> Array D Ix2 Char) -> Grid -> Int
calculateAll transform g = sum $ do
    pg <- splitSpaceColumns g
    let (digits, op) = breakOperator pg
    pure $ calculate (transform digits) op

main :: IO ()
main = do
    grid <- readInputGrid 6
    print $ calculateAll id grid
    print $ calculateAll A.transpose grid
