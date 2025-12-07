module Main (main) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import ReadInput (Parser, parseInput)

data Inventory = Inventory { invFreshRanges :: [(Int, Int)], invAvailableIds :: [Int] }
    deriving Show

inventoryP :: Parser Inventory
inventoryP = Inventory <$> (rangeP `P.sepEndBy` P.newline) <* P.newline
                       <*> (P.decimal `P.sepEndBy` P.newline)
  where
    rangeP = (,) <$> P.decimal <* P.char '-' <*> P.decimal

-- Invariant: in order of the keys, the values should alternate True, False, True, False ...
type IntervalSet = IntMap Bool

addInterval :: Int -> Int -> IntervalSet -> IntervalSet
addInterval lo hi _ | lo >= hi = error "addInterval: lo >= hi"
addInterval lo hi iset =
    (if includeLo then IM.insert lo' True else id) $
    (if includeHi then IM.insert hi' False else id) $
    IM.union keepL keepR
  where
    -- We will delete all entries in [lo', hi']
    (lo', includeLo) = case IM.lookupLE lo iset of
        Nothing -> (lo, True)
        Just (x, True) -> (x, True)
        -- If x == lo, we should remove the (x, False) entry since upper bound will be updated
        Just (x, False) -> (lo, x < lo)
    (hi', includeHi) = case IM.lookupGE hi iset of
        Nothing -> (hi, True)
        Just (x, True) -> (hi, x > hi)
        Just (x, False) -> (x, True)
    keepL = IM.takeWhileAntitone (< lo') iset
    keepR = IM.dropWhileAntitone (<= hi') iset

checkInterval :: Int -> IntervalSet -> Bool
checkInterval k iset = case IM.lookupLE k iset of
    Just (_, True) -> True
    _ -> False

getFreshSet :: Inventory -> IntervalSet
getFreshSet = foldl' (\acc (x, y) -> addInterval x (y + 1) acc) IM.empty . invFreshRanges

countFresh :: IntervalSet -> [Int] -> Int
countFresh fresh = length . filter (`checkInterval` fresh)

sizeOfIntervalSet :: IntervalSet -> Int
sizeOfIntervalSet iset = sum [if v then (-k) else k | (k, v) <- IM.toList iset]

main :: IO ()
main = do
    inventory <- parseInput 5 inventoryP
    let freshSet = getFreshSet inventory
    print $ countFresh freshSet (invAvailableIds inventory)
    print $ sizeOfIntervalSet freshSet
