module Main (main) where

import Data.Char (isLower)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import ReadInput (Parser, parseInput)

type Graph = HashMap Text [Text]

graphP :: Parser Graph
graphP = M.fromList <$> lineP `P.sepEndBy` P.newline
  where
    lineP = (,) <$> nodeP <* P.string ": " <*> nodeP `P.sepBy` P.char ' '
    nodeP = P.takeWhile1P (Just "node") isLower

countPaths :: Graph -> Text -> Text -> Int
countPaths graph from to = lookupTable from
  where
    memoTable = M.insert to 1 $ M.fromList [(node, countFrom node) | node <- M.keys graph]
    lookupTable node = fromMaybe 0 (M.lookup node memoTable)
    countFrom node = case M.lookup node graph of
        Nothing -> 0
        Just nbrs -> sum (map lookupTable nbrs)

countMulti :: Graph -> [Text] -> Int
countMulti _ [] = 0
countMulti graph (step : steps) = product $ zipWith (countPaths graph) (step : steps) steps

main :: IO ()
main = do
    graph <- parseInput 11 graphP
    print $ countPaths graph "you" "out"
    print $ countMulti graph ["svr", "fft", "dac", "out"] +
            countMulti graph ["svr", "dac", "fft", "out"]
