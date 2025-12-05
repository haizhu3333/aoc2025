module Main (main) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import ReadInput (Parser, parseInput)

rangesP :: Parser [(Int, Int)]
rangesP = rangeP `P.sepBy` P.char ',' <* P.newline
  where
    rangeP = (,) <$> P.decimal <* P.char '-' <*> P.decimal

numDigits :: Int -> Int
numDigits value | value <= 0 = error "numDigits: positive int expected"
numDigits value = go 0 value
  where
    go !n 0 = n
    go !n x = go (n + 1) (x `div` 10)

-- | @multiplier r d@ is a multiplier for repeating a @d@-digit number @r@ times, e.g.
--
-- > multiplier 3 2 = 10101
multiplier :: Int -> Int -> Int
multiplier r d = sum [10 ^ (i * d) | i <- [0 .. r-1]]

-- | @nextPartial r x@ is the smallest number @n@, such that @n@ repeated @r@ times is
-- | greater than or equal to @x@.
{-
If d is a multiple of r, the next multiple of m is good (this will never go beyond d digits,
since 999... is always a multiple of m).
Otherwise, there cannot be such a number with the same number of digits, so round up d and find
the smallest multiple of m with the right number of digits.

prevPartial works similarly, just rounding down instead.
-}
nextPartial :: Int -> Int -> Int
nextPartial r x | dRem == 0 = (x + m - 1) `div` m
                | otherwise = 10 ^ pd
  where
    d = numDigits x
    (pd, dRem) = d `divMod` r
    m = multiplier r pd

-- | @prevPartial r x@ is the largest number @n@, such that @n@ repeated @r@ times is
-- | less than or equal to @x@.
prevPartial :: Int -> Int -> Int
prevPartial r x | dRem == 0 = x `div` m
                | otherwise = 10 ^ pd - 1
  where
    d = numDigits x
    (pd, dRem) = d `divMod` r
    m = multiplier r pd

-- | @duplicate r x@ is @x@ replicated @r@ times, e.g.
--
-- > duplicate 3 123 = 123123123
duplicate :: Int -> Int -> Int
duplicate r x = multiplier r (numDigits x) * x

idsDupedTimes :: Int -> Int -> Int -> IntSet
idsDupedTimes r lo hi = IS.fromList $ map (duplicate r) [nextPartial r lo .. prevPartial r hi]

idsDupedAnyTimes :: Int -> Int -> IntSet
idsDupedAnyTimes lo hi = IS.unions [idsDupedTimes r lo hi | r <- [2 .. rMax]]
  where
    rMax = numDigits hi

sumDuped :: (Int -> Int -> IntSet) -> [(Int, Int)] -> Int
sumDuped dupf = IS.foldl' (+) 0 . IS.unions . map (uncurry dupf)

main :: IO ()
main = do
    ranges <- parseInput 2 rangesP
    print $ sumDuped (idsDupedTimes 2) ranges
    print $ sumDuped idsDupedAnyTimes ranges
