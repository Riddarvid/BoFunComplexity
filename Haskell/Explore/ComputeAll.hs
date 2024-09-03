{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Explore.ComputeAll () where
import           All                (AscOrder, BDD, ItemOrder, Poly (P), false,
                                     genAlgThinMemoPoly, xP)
import           BDD.Examples       (pick, true)
import           BoFun
import           Computing          (computeMin)
import           Data.Either        (isLeft)
import           Data.Maybe         (isJust)
import           Data.Set           (Set, fromList, size, toList)
import           DSLsofMath.Algebra (AddGroup, MulGroup, Multiplicative (one))
import           PiecewisePoly      (PiecewisePoly, linearizePW, minPWs,
                                     piecewiseFromPoly, showPW)

test :: IO ()
test = mapM_ print (genAllPPs 4)

-- Counts the number of pieces of a piecewise polynomial.
-- linearizePW results in a list of either pieces or separations,
-- thus we can count the number of pieces by filtering on isLeft.
countPieces :: (AddGroup a, MulGroup a, Eq a) => PiecewisePoly a -> Int
countPieces pw = length $ filter isLeft $ linearizePW pw

-- Takes a set of Polynomials and creates the piecewise polynomial that is the minimum
-- of the polynomials at each point.
polySetToPW :: (Show a, AddGroup a, MulGroup a, Ord a) => Set (Poly a) -> PiecewisePoly a
polySetToPW polys = minPWs $ map piecewiseFromPoly $ toList polys

predPP :: Int -> Set a -> Bool
predPP n s = size s >= n

uniquePPs :: [([Bool], Set (Poly Rational))] -> Set (Set (Poly Rational))
uniquePPs xs = fromList $ map snd xs

findLarge :: Int -> Int -> [([Bool], Set (Poly Rational))]
findLarge pieces bits = filter (\(_, pp) -> predPP pieces pp) $ genAllPPs bits

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

test2 :: String
test2 = showPW ppEx

ppEx :: PiecewisePoly Rational
ppEx = minPWs $ map piecewiseFromPoly [p1, p2]
  where
    p1 = P [1, 1]
    p2 = P [2, -1]
