module Main (main) where

import Data.Char (digitToInt)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import ReadInput (readInputText)

getDigits :: Text -> [Vector Int]
getDigits = map convertLine . T.lines
  where
    convertLine line = V.map digitToInt $ V.fromListN (T.length line) $ T.unpack line

maxDigits :: Int -> Vector Int -> Int
maxDigits nDigits digits | nDigits > len = error "not enough digits"
                         | otherwise = go 0 0 nDigits
  where
    len = V.length digits
    go !acc _ 0 = acc
    go !acc start nD =
        let valid = V.slice start (len - start - nD + 1) digits
            highPos = V.maxIndex valid + start
            high = digits V.! highPos
        in go (acc * 10 + high) (highPos + 1) (nD - 1)

main :: IO ()
main = do
    input <- getDigits <$> readInputText 3
    print $ sum $ map (maxDigits 2) input
    print $ sum $ map (maxDigits 12) input
