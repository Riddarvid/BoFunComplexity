{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Explore.Compute3Bit () where
import           All           (AscOrder, BDD, ItemOrder, Poly, false,
                                genAlgThinMemoPoly)
import           BDD.Examples  (pick, true)
import           BoFun
import           Computing     (computeMin)
import           Data.Maybe    (isJust)
import           Data.Set      (Set)
import           PiecewisePoly (PiecewisePoly)

test :: IO ()
test = mapM_ print interestingPPs

interestingPPs :: [([Bool], Set (Poly Rational))]
interestingPPs = filter (\(_, pp) -> not $ null pp) result
  where
    result = map (\out -> (out, genAlgThinMemoPoly 3 (mkBDD $ map boolToBDD out))) all3bitOutputs

boolToBDD :: Bool -> BDD AscOrder
boolToBDD True  = true
boolToBDD False = false

mkBDD :: [BDD AscOrder] -> BDD AscOrder
mkBDD output = pick 0
    (pick 1
      (pick 2 (output !! 7) (output !! 6))
      (pick 2 (output !! 5) (output !! 4))
    )
    (pick 1
      (pick 2 (output !! 3) (output !! 2))
      (pick 2 (output !! 1) (output !! 0))
    )

all3bitOutputs :: [[Bool]]
all3bitOutputs = outputPermutations 3

outputPermutations :: Int -> [[Bool]]
outputPermutations n = permutations (2^n)

permutations :: Int -> [[Bool]]
permutations 0 = [[]]
permutations n = do
  v <- [False, True]
  vs <- permutations (n - 1)
  return (v : vs)

