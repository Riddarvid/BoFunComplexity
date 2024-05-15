module Computing (computeMin) where
import BoFun (BoFun (..))
import Data.Monoid (Endo(..))
import Data.Maybe (isJust)
import DSLsofMath.Algebra
    ( Multiplicative((*), one), Additive((+), zero), (-) )
import Prelude hiding ((+), (-), (*))
import Control.Arrow((>>>))
import Data.Function.Memoize (Memoizable(..), traceMemoize, memoFix)
import PiecewisePoly ( PiecewisePoly, minPWs )
import Data.Function (fix)


computeMinStep :: (Show f, BoFun f i) => Endo (f -> PiecewisePoly Rational)
computeMinStep = Endo $ \recCall fun -> if isJust (isConst fun)
  then zero
  else one + minPWs $ do
    i <- variables fun
    let
      [a, b] = do
        (value, factor) <- [(False, mempty), (True, one - mempty)]
        return $ factor * recCall (setBit (i, value) fun)
    return $ a + b

-- QUESTION: What are the minimum requirements to be able to use computeMin?
-- Which typeclasses do we have to implement?

computeMin :: (Show f, BoFun f i, Memoizable f) => f -> PiecewisePoly Rational
computeMin = fix $ appEndo computeMinStep >>> memoize