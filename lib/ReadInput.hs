{-# LANGUAGE NondecreasingIndentation #-}
module ReadInput (
    Parser, readInputText, parseInput
) where

import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as T
import Data.Void (Void)
import qualified Text.Megaparsec as P
import Text.Printf (printf)
import Paths_aoc2025 (getDataFileName)

type Parser = P.Parsec Void Text

dayPath :: Int -> FilePath
dayPath = printf "inputs/%02d.txt"

readInputText :: Int -> IO Text
readInputText day = do
    path <- getDataFileName $ dayPath day
    T.readFile path

parseInput :: Int -> Parser a -> IO a
parseInput day parser = do
    text <- readInputText day
    case P.parse (parser <* P.eof) (dayPath day) text of
        Left errorBundle -> fail $ P.errorBundlePretty errorBundle
        Right result -> pure result
