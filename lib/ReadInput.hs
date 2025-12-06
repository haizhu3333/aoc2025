{-# LANGUAGE NondecreasingIndentation #-}
module ReadInput (
    Parser, readInputText, parseInput,
    Grid, readInputGrid
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr)
import Data.Massiv.Array (Matrix, U)
import qualified Data.Massiv.Array as A
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Void (Void)

import qualified Text.Megaparsec as P
import Text.Printf (printf)
import Paths_aoc2025 (getDataFileName)

type Parser = P.Parsec Void Text

dayPath :: Int -> FilePath
dayPath = printf "inputs/%02d.txt"

readInputBytes :: Int -> IO ByteString
readInputBytes day = do
    path <- getDataFileName $ dayPath day
    B.readFile path

readInputText :: Int -> IO Text
readInputText day = T.decodeUtf8 <$> readInputBytes day

parseInput :: Int -> Parser a -> IO a
parseInput day parser = do
    text <- readInputText day
    case P.parse (parser <* P.eof) (dayPath day) text of
        Left errorBundle -> fail $ P.errorBundlePretty errorBundle
        Right result -> pure result

type Grid = Matrix U Char

readInputGrid :: Int -> IO Grid
readInputGrid day = do
    bytes <- readInputBytes day
    arr <- A.stackOuterSlicesM $ map lineToArray $ B.lines bytes
    A.computeIO arr
  where
    lineToArray = A.map (chr . fromIntegral) . A.castFromByteString A.Seq
