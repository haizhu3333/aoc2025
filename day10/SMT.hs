module SMT (natTarget, boolTarget, solve) where

-- This module generates SMTLIB code and invokes z3 on the command line.
-- Could also use the 'z3' library but it uses an older version of libz3,
-- and this seems easier.

import Data.Bool (bool)
import Control.Monad (forM_, unless)
import System.Exit (ExitCode(..))
import System.IO (Handle, hGetLine, hClose)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)
import Text.Printf (printf, hPrintf)

data TargetConf a = TargetConf
    { varSort :: String
    , varAssert :: Maybe (String -> String)
    , varFunction :: String
    , toLiteral :: a -> String
    }

natTarget :: TargetConf Int
natTarget = TargetConf
    { varSort = "Int"
    , varAssert = Just (printf "(>= %s 0)")
    , varFunction = "+"
    , toLiteral = show
    }

boolTarget :: TargetConf Bool
boolTarget = TargetConf
    { varSort = "Bool"
    , varAssert = Nothing
    , varFunction = "xor"
    , toLiteral = bool "false" "true"
    }

addToIndices :: Int -> [Int] -> [[Int]] -> [[Int]]
addToIndices x = go 0
  where
    go !_ [] rss = rss
    go !_ _ [] = error "Index out of bounds"
    go !n (i : is) (rs : rss)
        | i == n    = (x : rs) : go (n + 1) is rss
        | otherwise = rs : go (n + 1) (i : is) rss

transposeIndices :: Int -> [[Int]] -> [[Int]]
transposeIndices nCounts buttons =
    foldr (uncurry addToIndices) (replicate nCounts []) (zip [0..] buttons)

printInstance :: Handle -> TargetConf a -> [[Int]] -> [a] -> IO ()
printInstance h TargetConf{..} buttons targets = do
    let varIds = [0 .. length buttons - 1]
        nEqns = length targets
        eqnLhsIndices = transposeIndices nEqns buttons
        var = printf "x%d" :: Int -> String
    forM_ varIds $ \i ->
        hPrintf h "(declare-const %s %s)\n" (var i) varSort
    case varAssert of
        Nothing -> pure ()
        Just assertFn -> forM_ varIds $ \i ->
            hPrintf h "(assert %s)\n" (assertFn (var i))
    forM_ (zip eqnLhsIndices targets) $ \(lhsIndices, target) -> do
        let varArgs = unwords $ map var lhsIndices
        hPrintf h "(assert (= (%s %s) %s))\n" varFunction varArgs (toLiteral target)
    hPrintf h "(define-const total (Int) (+ %s))\n" (unwords $ map var varIds)
    hPrintf h "(minimize total)\n"
    hPrintf h "(check-sat)\n"
    hPrintf h "(eval total)\n"

solve :: TargetConf a -> [[Int]] -> [a] -> IO Int
solve conf buttons targets = do
    (Just pIn, Just pOut, _, ph) <- createProcess (proc "z3" ["-smt2", "-in"])
        { std_in = CreatePipe, std_out = CreatePipe }
    printInstance pIn conf buttons targets
    hClose pIn
    exitCode <- waitForProcess ph
    unless (exitCode == ExitSuccess) $
        fail $ printf "Process exited with code %s" (show exitCode)
    line1 <- hGetLine pOut
    unless (line1 == "sat") $
        fail $ printf "Expected sat, got %s" line1
    line2 <- hGetLine pOut
    hClose pOut
    pure $ read line2
