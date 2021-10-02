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
import System.Random.MWC
import System.Random.Stateful

type Verbosity = Int

type ParseFun a = [Token] -> Err a

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree =
  do
    putStrV v $ "\nAbstract Syntax:\n" ++ show tree

execPure :: RandomGen g => Exp -> g -> Integer
execPure p rng = fst $ interpretPure p rng

interpretPure :: RandomGen g => Exp -> g -> (Integer, g)
interpretPure (ERoll (DiceRoll str)) rng = multiRoll qty size rng
  where
    (Just index) = 'd' `elemIndex` str
    qty = read $ take index str
    size = read $ drop (index + 1) str
interpretPure (EModifier n) rng = (n, rng)
interpretPure (EMinus exp1 exp2) rng = (result1 - result2, rng)
  where
    (result1, rng1) = interpretPure exp1 rng
    (result2, rng2) = interpretPure exp2 rng1
interpretPure (EPlus exp1 exp2) rng = (result1 + result2, rng2)
  where
    (result1, rng1) = interpretPure exp1 rng
    (result2, rng2) = interpretPure exp2 rng1

-- Rolls a die of a given size 1 or more times, summing the results
multiRoll :: RandomGen g => Integer -> Integer -> g -> (Integer, g)
multiRoll 1 size rng = randomR (1, size) rng
multiRoll qty size rng = (rollResult + preSumResult, rngResult)
  where
    (rollResult, nextRng) = randomR (1, size) rng
    (preSumResult, rngResult) = multiRoll (qty - 1) size nextRng

execM :: StatefulGen g m => Exp -> g -> m Integer
execM = interpretM

interpretM :: StatefulGen g m => Exp -> g -> m Integer
interpretM (ERoll (DiceRoll str)) rng = sum <$> replicateM qty (uniformRM (1, size) rng)
  where
    (Just index) = 'd' `elemIndex` str
    qty = read $ take index str
    size = read $ drop (index + 1) str
interpretM (EModifier n) rng = return n
-- We could also use liftA2 from Control.Applicative
interpretM (EPlus exp1 exp2) rng = (+) <$> interpretM exp1 rng <*> interpretM exp2 rng
interpretM (EMinus exp1 exp2) rng = (-) <$> interpretM exp1 rng <*> interpretM exp2 rng

-- TODO(Chris): Modify this function to work with either pure or monadic RNG
-- interfaces
run :: Verbosity -> ParseFun Exp -> String -> IO ()
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

          putStrLn "\nResult Tree:"
          -- rng <- getStdGen
          -- putStrLn $ printTree $ execPure tree rng
          rng <- createSystemRandom
          result <- evalExpM tree rng
          -- putStrLn $ printTree result
          print result

          putStrLn "\nTotal:"
          print $ collapse result

          exitSuccess
        Left _ -> exitFailure
        Right _ -> exitFailure

main :: IO ()
main = do
  input <- getLine
  run 2 pExp input