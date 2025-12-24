module Main (main) where

import Control.Applicative ((<|>), some)
import qualified Control.Foldl as F
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import ReadInput (Parser, parseInput)
import Text.Printf (printf)

newtype Piece = Piece [[Bool]]
    deriving Show

data Region = Region { rWidth :: !Int, rHeight :: !Int, rPieceCounts :: [Int] }
    deriving Show

pieceP :: Parser Piece
pieceP = (P.decimal :: Parser Int) *> P.char ':' *> P.newline *>
         (Piece <$> cellsP) <* P.newline
  where
    cellsP = some cell `P.sepEndBy1` P.newline
    cell = False <$ P.char '.' <|> True <$ P.char '#'

regionP :: Parser Region
regionP = Region <$> P.decimal <* P.char 'x'
                 <*> P.decimal <* P.string ": "
                 <*> P.decimal `P.sepBy` P.char ' ' <* P.newline

inputP :: Parser ([Piece], [Region])
inputP = (,) <$> some (P.try pieceP) <*> some regionP

pieceSize :: Piece -> Int
pieceSize (Piece cs) = sum $ map (length . filter id) cs

pieceBBox :: Piece -> (Int, Int)
pieceBBox (Piece cs) = (maximum $ map length cs, length cs)

maxBBox :: [Piece] -> (Int, Int)
maxBBox ps = case F.fold calcMax ps of
    (Just bw, Just bh) -> (bw, bh)
    _ -> error "maxBBox: empty list"
  where
    calcMax = F.premap pieceBBox $ (,) <$> F.premap fst F.maximum
                                       <*> F.premap snd F.maximum

hasEnoughSpace :: [Int] -> Region -> Bool
hasEnoughSpace sizes Region{..} = required <= rWidth * rHeight
  where
    required = sum $ zipWith (*) sizes rPieceCounts

fitsBBox :: (Int, Int) -> Region -> Bool
fitsBBox (bw, bh) Region{..} = sum rPieceCounts <= div rWidth bw * div rHeight bh

countRegions :: [Piece] -> [Region] -> (Int, Int)
countRegions pieces = F.fold counter
  where
    sizes = map pieceSize pieces
    bbox = maxBBox pieces
    counter = (,) <$> F.prefilter (fitsBBox bbox) F.length
                  <*> F.prefilter (hasEnoughSpace sizes) F.length

main :: IO ()
main = do
    (pieces, regions) <- parseInput 12 inputP
    let (fits, possible) = countRegions pieces regions
    printf "Fits: %d | Possibly fits: %d\n" fits possible
    putStrLn "If these two do not agree, a much more complex approach will be needed!"
