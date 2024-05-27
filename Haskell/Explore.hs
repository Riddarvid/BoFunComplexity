{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Explore (main) where

import BoFun (BoFun (..))
import PiecewisePoly (showPW)
import Computing (computeMin)
import Data.Function.Memoize (Memoizable (memoize), deriveMemoizable)
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)

-- QUESTION: Eftersom funktionen är symmetrisk kan det ju per definition inte finnas
-- någon evalueringsordning som är bättre än någon annan. Resonerar vi koorekt här?
-- Annan approach: results :: (Int -> Bool). Blir detta problematiskt med memoize?
-- TODO: Kolla upp om det finns representation för (Fin n -> Bool)?
-- Potentiellt: Wrappa en (Int -> a) i en ny konstruktur/typ, skriv Memoizable 
-- instans för den nya typen.
data Symmetric = Symmetric {
  ones :: Int, -- the number of ones we've encountered so far
  bitsLeft :: Int, -- the number of bits not yet evaluated
  results :: [Bool] -- Mapping between number of ones and actual result.
}

instance Show Symmetric where
  show :: Symmetric -> String
  show f = "Ones: " ++ show (ones f) ++ "\nBits left: " ++ show (bitsLeft f)

instance BoFun Symmetric () where
  isConst :: Symmetric -> Maybe Bool
  isConst f
    | bitsLeft f == 0 = Just (results f !! ones f) -- TODO cleanup
    | otherwise = Nothing
-- TODO: Vi kan veta om f är konstant tidigare om listan endast innehåller ett av true/false
-- Ex: AND2 där vi har stött på en 0:a blir alltid false

  variables :: Symmetric -> [()]
  variables f = replicate (bitsLeft f) ()

  setBit :: ((), Bool) -> Symmetric -> Symmetric
  setBit (_, v) f
    | v = f{ones = ones f + 1, bitsLeft = bitsLeft f - 1}
    | otherwise = f{bitsLeft = bitsLeft f - 1}

-- Ny representation: Endast bitsLeft och en lista som krymper varje gång vi sätter en bit.
-- Bra approach: Med en representering: Jämför antalet värden av vår datatyp med antalet funktioner
-- i den klass vi vill representera.

-- Memoizable instance
$(deriveMemoizable ''Symmetric)

-- Examples

mkSymmetric :: Int -> (Int -> Bool) -> Symmetric
mkSymmetric nBits eval = Symmetric {ones = 0, bitsLeft = nBits, results = map eval [0 .. nBits]}

sumMod2 :: Symmetric
sumMod2 = mkSymmetric 50 (\n -> n `mod` 2 == 1)

main :: IO ()
main = do
  putStrLn $ "sumMod2, 1 bit: " ++ showPW (computeMin sumMod2)