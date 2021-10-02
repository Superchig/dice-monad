module DirectInterpret where

import AbsDiceExpr
import Control.Monad
import Data.List
import System.Random.Stateful

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