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

execCBN :: Program -> Integer
execCBN (Prog p) = interpret p

interpret :: Exp -> Integer
interpret (ERoll (DiceRoll str)) = qty + size
  where
    (Just index) = 'd' `elemIndex` str
    qty = read $ take index str
    size = read $ drop (index + 1) str
interpret (EModifier n) = n
interpret (EPlus exp1 exp2) = interpret exp1 + interpret exp2

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
          putStrLn $ printTree $ execCBN tree

          exitSuccess
        Left _ -> exitFailure
        Right _ -> exitFailure

-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms
-- main :: IO ()
-- main = do
--   g <- getStdGen
--   -- print $ take 10 (randoms g :: [Double])
--   let res = randomR (1, 20) g :: (Integer, StdGen)
--   print $ fst res

main :: IO ()
main = do
  input <- getLine
  run 2 pProgram input