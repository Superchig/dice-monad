module Main where

import AbsDiceExpr
import Control.Monad
import Data.List
import ErrM
import LexDiceExpr
import Lib
import ParDiceExpr
import PrintDiceExpr
import System.Exit
import System.Random

type Verbosity = Int

type ParseFun a = [Token] -> Err a

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree =
  do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree

execCBN :: RandomGen g => Program -> g -> Integer
execCBN (Prog p) rng = fst $ interpret p rng

interpret :: RandomGen g => Exp -> g -> (Integer, g)
interpret (ERoll (DiceRoll str)) rng = multiRoll qty size rng
  where
    (Just index) = 'd' `elemIndex` str
    qty = read $ take index str
    size = read $ drop (index + 1) str
interpret (EModifier n) rng = (n, rng)
interpret (EMinus exp1 exp2) rng = (result1 - result2, rng)
  where
    (result1, rng1) = interpret exp1 rng
    (result2, rng2) = interpret exp2 rng1
interpret (EPlus exp1 exp2) rng = (result1 + result2, rng2)
  where
    (result1, rng1) = interpret exp1 rng
    (result2, rng2) = interpret exp2 rng1

-- Rolls a die of a given size 1 or more times, summing the results
multiRoll :: RandomGen g => Integer -> Integer -> g -> (Integer, g)
multiRoll 1 size rng = randomR (1, size) rng
multiRoll qty size rng = (rollResult + preSumResult, rngResult)
  where
    (rollResult, nextRng) = randomR (1, size) rng
    (preSumResult, rngResult) = multiRoll (qty - 1) size nextRng

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s =
  let ts = myLexer s
   in case p ts of
        Bad s -> do
          putStrLn "\nParse Failed...\n"
          putStrV v "Tokens:"
          putStrV v $ show ts
          putStrLn s
          exitFailure
        Ok tree -> do
          putStrLn "\nInput:"
          putStrLn $ printTree tree

          showTree v tree
          putStrLn "\nTokens:"
          putStrV v $ show ts

          putStrLn "\nOutput:"
          rng <- getStdGen
          putStrLn $ printTree $ execCBN tree rng

          exitSuccess
        Left _ -> exitFailure
        Right _ -> exitFailure

main :: IO ()
main = do
  input <- getLine
  run 2 pProgram input