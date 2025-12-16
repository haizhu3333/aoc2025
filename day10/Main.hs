module Main (main) where

import Control.Applicative ((<|>), many)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import ReadInput (Parser, parseInput)
import SMT (solve, boolTarget, natTarget)

data Machine = Machine
    { lights :: [Bool]
    , buttons :: [[Int]]
    , joltage :: [Int]
    } deriving Show

machinesP :: Parser [Machine]
machinesP = machineP `P.sepEndBy` P.newline
  where
    machineP = Machine <$> lightsP <*> buttonsP <*> joltageP
    lightsP = P.between (P.char '[') (P.string "] ") $ many lightP
    lightP = False <$ P.char '.' <|> True <$ P.char '#'
    buttonsP = P.between (P.char '(') (P.char ')') numList `P.sepEndBy` P.char ' '
    joltageP = P.between (P.char '{') (P.char '}') numList
    numList = P.decimal `P.sepBy` P.char ','

solveLights :: Machine -> IO Int
solveLights m = solve boolTarget (buttons m) (lights m)

solveCounters :: Machine -> IO Int
solveCounters m = solve natTarget (buttons m) (joltage m)

main :: IO ()
main = do
    machines <- parseInput 10 machinesP
    print . sum =<< mapM solveLights machines
    print . sum =<< mapM solveCounters machines
