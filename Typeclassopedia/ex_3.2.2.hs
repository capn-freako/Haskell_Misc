-- Typeclassopedia - Sec. 3.2, Exercise 2.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   September 20, 2015
--
-- Copyright (c) 2015 David Banas; all rights reserved World wide.
--
-- Discussion, as per exercise instructions:
--  Similarities: Both types/instances return a pair of values.
--  Differences:
--    - The type of the first member of the pair can differ from that
--      of the second, in the '(,) e' case.
--    - The mapped function, g, is only mapped over the second member
--      of the pair, in the '(,) e' case.
--    - The type of the second member of the pair is NOT included in
--      the Functor instance, for either type, but the type of the
--      first member IS included, for ((,) e), but NOT for Pair.

{-# LANGUAGE TemplateHaskell #-}

module Main where

-- import Test.QuickCheck
import Control.Monad (unless)
import System.Exit (exitFailure)
import System.Random (Random)
import Test.QuickCheck (choose, vectorOf, elements, collect)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)

class MyFunctor f where
    fmap' :: (a -> b) -> f a -> f b

-- Functor ((,) e)
instance MyFunctor ((,) e) where
    fmap' g (e, a) = (e, g a)

data Pair a = Pair a a deriving (Eq, Show)

instance (Num a, Random a) => Arbitrary (Pair a) where
    arbitrary = do
        x <- choose (0, 99)
        y <- choose (0, 99)
        return (Pair x y)

-- Functor (Pair a)
instance MyFunctor Pair where
    fmap' g (Pair x y) = Pair (g x) (g y)

-- Testing Functor law on defined instances.
prop_comma_test testVal = fmap' id testVal == testVal  -- "id testVal" was producing an annoying error.
    where types = testVal :: (String, Int)

prop_pair_test testVal = fmap' id testVal == testVal
    where types = testVal :: Pair Int

return []  -- See comments above 'quickCheckAll', in http://hackage.haskell.org/package/QuickCheck-2.8/docs/src/Test-QuickCheck-All.html
runTests = $quickCheckAll 
-- runTests = $verboseCheckAll 

main :: IO ()
main = do
    allPass <- runTests
    unless allPass exitFailure

