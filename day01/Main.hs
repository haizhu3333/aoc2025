module Main (main) where

import Control.Applicative ((<|>))
import Data.List (scanl', mapAccumL)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import ReadInput (Parser, parseInput)

instructionsP :: Parser [Int]
instructionsP = instrP `P.sepEndBy` P.newline
  where
    instrP = dirP <*> P.decimal
    dirP = (id <$ P.char 'R') <|> (negate <$ P.char 'L')

rotate :: [Int] -> [Int]
rotate = scanl' (\x y -> (x + y) `mod` 100) 50

zeroStops :: [Int] -> Int
zeroStops = length . filter (== 0) . rotate

zeroPasses1 :: Int -> Int -> (Int, Int)
zeroPasses1 start instr = (end, passes)
  where
    end = (start + instr) `mod` 100
    passes = case compare instr 0 of
        LT -> (abs instr - (if start == 0 then 100 else start)) `div` 100 + 1
        EQ -> 0
        GT -> (instr + start) `div` 100

zeroPasses :: [Int] -> Int
zeroPasses = sum . snd . mapAccumL zeroPasses1 50

main :: IO ()
main = do
    instrs <- parseInput 1 instructionsP
    print $ zeroStops instrs
    print $ zeroPasses instrs
