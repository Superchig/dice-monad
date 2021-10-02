module Lib
  ( someFunc,
    ResultExp,
    evalExpM,
    collapse,
  )
where

import AbsDiceExpr
import Control.Monad
import Data.List
import System.Random.Stateful

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ResultExp
  = RPlus ResultExp ResultExp
  | RMinus ResultExp ResultExp
  -- Size of roll and then rolls
  | RRoll Integer [Integer]
  | RModifier Integer
  deriving (Show)

-- Evaluates an Exp and returns a ResultExp tree with information on individual
-- rolls.
evalExpM :: StatefulGen g m => Exp -> g -> m ResultExp
evalExpM (ERoll (DiceRoll str)) rng = RRoll size <$> replicateM qty (uniformRM (1, size) rng)
  where
    -- Ad-hoc parsing of qty and size from str, since I haven't figured out how
    -- to get bnfc to do this for me (while still allowing for no-whitespace
    -- expressions like 1d20)
    (Just index) = 'd' `elemIndex` str
    qty = read $ take index str
    size = read $ drop (index + 1) str
evalExpM (EModifier n) _ = return $ RModifier n
evalExpM (EPlus exp1 exp2) rng = RPlus <$> evalExpM exp1 rng <*> evalExpM exp2 rng
evalExpM (EMinus exp1 exp2) rng = RMinus <$> evalExpM exp1 rng <*> evalExpM exp2 rng

collapse :: ResultExp -> Integer
collapse (RPlus rexp1 rexp2) = collapse rexp1 + collapse rexp2
collapse (RMinus rexp1 rexp2) = collapse rexp1 - collapse rexp2
collapse (RRoll _ rolls) = sum rolls
collapse (RModifier n) = n