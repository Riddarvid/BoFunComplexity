module Explore () where

import Main (computeMin)
import PiecewisePoly

data MyFunction = MyFunction
  deriving (Show)

--instance BoFun MyFunction () where
--  isConst

myFun :: MyFunction
myFun = MyFunction

-- myPoly :: PiecewisePoly Rational
-- myPoly = computeMin myFun