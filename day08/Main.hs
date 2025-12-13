{-# LANGUAGE DerivingVia #-}
module Main (main) where

import Data.Coerce (coerce)
import Data.Equivalence.Monad (EquivM, runEquivM, classDesc, classes, desc, equate)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.Semigroup (Sum(..))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Geometry (V3(..), distance2, getComponent)
import ReadInput (Parser, parseInput)

data Pair = Pair { index1 :: !Int, index2 :: !Int, dist2 :: !Int }
    deriving (Eq, Show)

newtype Circuit = Circuit { circuitSize :: Int }
    deriving (Eq, Ord, Show)
    deriving (Semigroup) via (Sum Int)

type Point = V3 Int

pointsP :: Parser (Vector Point)
pointsP = V.fromList <$> pointP `P.sepEndBy` P.newline
  where
    pointP = V3 <$> P.decimal <* P.char ',' <*> P.decimal <* P.char ',' <*> P.decimal

makePairs :: Vector Point -> [Pair]
makePairs points = sortOn dist2 $ do
    i <- [0 .. V.length points - 1]
    j <- [i + 1 .. V.length points - 1]
    pure $ Pair i j (distance2 (points ! i) (points ! j))

runCircuitM :: (forall s. EquivM s Circuit Int a) -> a
runCircuitM = runEquivM (const $ Circuit 1) (<>)

connectAll :: [Pair] -> EquivM s Circuit Int ()
connectAll = mapM_ $ \(Pair i1 i2 _) -> equate i1 i2

getCircuits :: [Pair] -> [Circuit]
getCircuits ps = runCircuitM $ do
    connectAll ps
    cls <- classes
    mapM desc cls

topCircuitProduct :: Int -> [Pair] -> Int
topCircuitProduct nCircuit =
    product .
    coerce @[Circuit] @[Int] .
    take nCircuit .
    sortOn Down .
    getCircuits

connectUntil :: (Circuit -> Bool) -> [Pair] -> EquivM s Circuit Int Pair
connectUntil _ [] = error "No more connections to make"
connectUntil condition (p : ps) = do
    equate (index1 p) (index2 p)
    circuit <- classDesc (index1 p)
    if condition circuit
    then pure p
    else connectUntil condition ps

lastConnectionX :: Vector Point -> [Pair] -> Int
lastConnectionX points pairs = getComponent 0 (points ! i1) * getComponent 0 (points ! i2)
  where
    n = V.length points
    Pair i1 i2 _ = runCircuitM $ connectUntil ((== n) . circuitSize) pairs

main :: IO ()
main = do
    points <- parseInput 8 pointsP
    let pairs = makePairs points
    print $ topCircuitProduct 3 (take 1000 pairs)
    print $ lastConnectionX points pairs
