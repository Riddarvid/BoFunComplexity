{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Explore.ComputeAll () where
import           All           (AscOrder, BDD, ItemOrder, Poly, false,
                                genAlgThinMemoPoly)
import           BDD.Examples  (pick, true)
import           BoFun
import           Computing     (computeMin)
import           Data.Maybe    (isJust)
import           Data.Set      (Set)
import           PiecewisePoly (PiecewisePoly)

test :: IO ()
test = mapM_ print (genAllPPs 3)

genAllPPs :: Int -> [([Bool], Set (Poly Rational))]
genAllPPs n = map (\out -> (out, genAlgThinMemoPoly n (bddFromOutput n out))) $
  outputPermutations n

boolToBDD :: Bool -> BDD a
boolToBDD True  = true
boolToBDD False = false

bddFromOutput :: Int -> [Bool] -> BDD AscOrder
bddFromOutput bits = bddFromOutput' bits 0

bddFromOutput' :: ItemOrder a => Int -> Int -> [Bool] -> BDD a
bddFromOutput' 0 varN out = boolToBDD (out !! varN)
bddFromOutput' bits varN out = pick bits
  (bddFromOutput' (bits - 1) (2 * varN + 1) out)
  (bddFromOutput' (bits - 1) (2 * varN) out)

outputPermutations :: Int -> [[Bool]]
outputPermutations n = permutations (2^n)

permutations :: Int -> [[Bool]]
permutations 0 = [[]]
permutations n = do
  v <- [False, True]
  vs <- permutations (n - 1)
  return (v : vs)

