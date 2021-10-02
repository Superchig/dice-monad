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