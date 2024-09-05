module Main where

import           BoFun              ()
import           Computing          (computeMin)
import           Data.Maybe         (isJust)
import           Debug.Trace        (trace)
import qualified Explore.ComputeAll
import           PiecewisePoly      (PiecewisePoly, evalPW, showPW)
import           Threshold          (iteratedMaj3, iteratedMaj5)

{-
* separated by 0 % 1
* piece [4,4,6,9,-61,23,67,-64,16]
* separated by 1 % 1
-}
maj3_2 :: PiecewisePoly Rational
maj3_2 = computeMin $ Threshold.iteratedMaj3 2

{-
TODO: figure out where the bisect asymmetry of (0, 1/4) vs. (7/8, 15/16) comes from.
* separated by 0 % 1
* piece [8,8,12,14,61,36,102,-126,-10712,19365,17503,-77698,168601,-249980,-313643,1716199,-1770993,-1454571,5011281,-4174314,-399914,3794567,-3710286,1941192,-604912,106400,-8192]
* separated by root of [0,0,0,0,0,0,0,0,-12,212,-673,226,2710,-6134,4136,5423,-15446,17254,-11248,4448,-992,96,0] between 0 % 1 and 1 % 4
* piece [8,8,12,14,61,36,102,-126,-10700,19129,18600,-79246,165907,-236868,-331172,1713854,-1729723,-1517123,5050327,-4164943,-439980,3832237,-3730238,1947624,-606096,106496,-8192]
* separated by root of [0,0,0,0,4,-14,1,-12,175,-161,-998,3551,-4928,555,9156,-14384,7125,5845,-11816,8727,-3526,772,-72,0] between 1 % 4 and 3 % 8
* piece [8,8,12,14,61,44,58,-60,-10756,19529,17554,-80248,176679,-262924,-303248,1720090,-1794005,-1427025,5004749,-4197705,-363572,3766645,-3697136,1937340,-604264,106352,-8192]
* separated by root of [0,1,-3,2,-21,71,-24,-135,-7,689,-1376,1433,-932,385,-93,10,0] between 25 % 64 and 101 % 256
* piece [8,8,12,14,61,44,66,-97,-10708,19382,18285,-81106,174820,-258825,-298686,1697380,-1767324,-1425978,4961708,-4132148,-420508,3799684,-3710282,1940814,-604816,106392,-8192]
* separated by root of [0,0,0,0,-4,14,-5,-22,52,-89,-46,394,-358,-409,1198,-1162,288,665,-938,582,-184,24,0] between 101 % 256 and 51 % 128
* piece [8,8,12,14,61,52,14,21,-10786,19204,18897,-81904,174394,-255647,-302472,1695466,-1756074,-1441532,4971474,-4130334,-430714,3810502,-3716802,1943226,-605328,106440,-8192]
* separated by root of [0,0,0,0,-4,14,-5,-22,10,54,-120,138,-96,37,-6,0] between 13 % 32 and 7 % 16
* piece [8,8,12,14,61,52,34,-89,-10601,19034,19632,-83754,174889,-252057,-303907,1682551,-1729794,-1459317,4957064,-4082429,-489454,3855127,-3739252,1950536,-606728,106560,-8192]
* separated by root of [0,1,-3,2,2,-3,-91,112,1343,-4490,3579,4875,-4827,-28207,88578,-134667,133272,-92987,46697,-16660,4026,-592,40,0] between 7 % 16 and 15 % 32
* piece [8,8,12,14,62,49,36,-87,-10604,18943,19744,-82411,170399,-248478,-299032,1677724,-1758001,-1370739,4822397,-3949157,-582441,3901824,-3755912,1954562,-607320,106600,-8192]
* separated by root of [0,0,0,0,-5,-3,65,-70,44,-360,266,1908,-5269,5797,558,-12659,19383,-10089,-7650,16435,-12413,5074,-1116,104,0] between 15 % 32 and 1 % 2
* piece [8,8,12,14,67,42,-30,116,-10853,19461,18714,-83427,179218,-266721,-282727,1685702,-1803260,-1309225,4790486,-3970803,-529508,3855489,-3732235,1947152,-605996,106496,-8192]
* separated by root of [0,1,-1,-12,33,3,-146,111,263,524,-2371,-1977,13919,-10938,-25427,65039,-59004,10590,30521,-35995,20529,-6834,1276,-104,0] between 1 % 2 and 17 % 32
* piece [8,8,12,15,66,30,3,119,-10999,19572,18977,-82903,176847,-268698,-268808,1674764,-1828687,-1244186,4731482,-3960213,-498987,3819494,-3711706,1940318,-604720,106392,-8192]
* separated by root of [0,0,0,0,-5,-3,60,-73,-6,129,-533,698,607,-2126,1193,1379,-2524,1421,337,-1140,834,-288,40,0] between 17 % 32 and 9 % 16
* piece [8,8,12,15,71,18,-51,376,-11395,19704,19842,-85593,180062,-267378,-277502,1683949,-1827731,-1256123,4744359,-3964849,-502831,3826041,-3716252,1942136,-605128,106432,-8192]
* separated by root of [0,1,-3,2,-6,23,-17,-10,-53,233,-362,312,-161,47,-6,0] between 9 % 16 and 19 % 32
* piece [8,8,12,15,71,18,-11,151,-10970,19329,20737,-87768,180357,-263563,-271482,1648154,-1779126,-1256838,4655884,-3817044,-640476,3910596,-3751532,1951846,-606728,106552,-8192]
* separated by root of [0,1,-3,2,2,-3,-31,40,432,-1498,1694,-518,2437,-12453,26624,-33910,28933,-17183,7080,-1942,320,-24,0] between 77 % 128 and 155 % 256
* piece [8,8,12,15,73,10,-1,151,-10980,19273,20879,-86984,176497,-257179,-275906,1654064,-1808906,-1178684,4534816,-3691358,-732708,3959122,-3769576,1956370,-607416,106600,-8192]
* separated by root of [0,0,0,0,-5,-3,60,-73,-55,157,-78,-77,160,-133,57,-10,0] between 155 % 256 and 39 % 64
* piece [8,8,12,15,73,15,-8,90,-10824,19203,21198,-88564,177835,-253440,-284722,1658274,-1800465,-1194047,4543600,-3687109,-744967,3970623,-3775980,1958596,-607864,106640,-8192]
* separated by root of [0,-1,1,12,-34,7,103,-142,-10,183,-1029,3533,-3797,-5515,20552,-24068,8927,9794,-15863,10553,-3946,812,-72,0] between 5 % 8 and 3 % 4
* piece [8,8,12,17,69,-7,84,8,-11016,19693,20934,-88950,180259,-262564,-270062,1661710,-1852599,-1104807,4477610,-3688843,-693653,3917791,-3746982,1949080,-606096,106496,-8192]
* separated by root of [0,-1,8,-15,-19,101,-203,447,-422,-1733,6122,-6369,-4167,18583,-19895,3713,14304,-19078,12464,-4768,1024,-96,0] between 7 % 8 and 15 % 16
* piece [8,8,12,17,67,10,46,-15,-10795,19186,22031,-90241,177215,-248587,-288922,1659745,-1811266,-1163180,4504931,-3663948,-746113,3961797,-3768982,1955896,-607312,106592,-8192]
* separated by 1 % 1
-}
maj3_3 :: PiecewisePoly Rational
maj3_3 = computeMin $ Threshold.iteratedMaj3 3

{-
* separated by 0 % 1
* piece [3,3,3,-12,6]
* separated by 1 % 1
-}
maj5_1 :: PiecewisePoly Rational
maj5_1 = computeMin $ Threshold.iteratedMaj5 1

{-
* separated by 0 % 1
* piece [9,9,9,30,12,62,-14,816,-2143,-44004,169768,-291977,751873,-2494791,5464225,-8722210,13579067,-21830058,29475938,-29211477,20338155,-9697875,3027801,-559872,46656]
* separated by 1 % 1
-}
maj5_2 :: PiecewisePoly Rational
maj5_2 = computeMin $ Threshold.iteratedMaj5 2

-- Conjecture: the cost for a twofold iterated threshold is always given by a polynomial.

main :: IO ()
main = do
  mapM_ print Explore.ComputeAll.findSimplest
  -- putStrLn $ "maj5_1: " ++ showPW maj5_1
  -- putStrLn $ "maj5_2: " ++ showPW maj5_2
  -- putStrLn $ "maj3_2: " ++ showPW maj3_2
  -- putStrLn $ "maj3_3: " ++ showPW maj3_3

----------------
-- Preparing for some plotting (see Maj3_3.hs)

pw3_2 :: Double -> Double
pw3_2 p = Prelude.fromRational (evalPW maj3_2 (Prelude.toRational p))

pw3_3 :: Double -> Double
pw3_3 p = Prelude.fromRational (evalPW maj3_3 (Prelude.toRational p))

-- plot3_3 = plotFunc [] (linearScale 1000 (0,1)) pw3_3
