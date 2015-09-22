-- Typeclassopedia - Sec. 3.2, Exercise 5.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   September 21, 2015
--
-- Copyright (c) 2015 David Banas; all rights reserved World wide.
--
-- Answer: Statement is true. Code, below, demonstrates this.

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.Random
import Test.QuickCheck (choose, vectorOf, elements, collect)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)

class MyFunctor f where
    fmap' :: (a -> b) -> f a -> f b

data Func1 a = Func1 a deriving (Show, Eq)
data Func2 a = Func2 a deriving (Show, Eq)

instance MyFunctor Func1 where
    fmap' g (Func1 x) = Func1 (g x)
instance MyFunctor Func2 where
    fmap' g (Func2 x) = Func2 (g x)

instance (Num a, Random a) => Arbitrary (Func1 a) where
    arbitrary = do
        x <- choose (0, 99)
        return $ Func1 x

instance (Num a, Random a) => Arbitrary (Func2 a) where
    arbitrary = do
        x <- choose (0, 99)
        return $ Func2 x

instance Num a => Num (Func1 a) where
    Func1 x + Func1 y = Func1 (x + y)
    Func1 x - Func1 y = Func1 (x - y)
    Func1 x * Func1 y = Func1 (x * y)
    abs (Func1 x)    = Func1 (abs x)
    signum (Func1 x) = Func1 (signum x)
    fromInteger x = Func1 (fromInteger x)

instance (Num a, Ord a, Random a) => Random (Func1 a) where
    randomR (Func1 x, Func1 y) g = let (res, g') = next g in
        let res' = fromIntegral res in
          (Func1 res', g')
--          if (res' >= x) && (res' <= y)
--            then (Func1 res', g')
--            else randomR (Func1 x, Func1 y) g'

    random g = let (res, g') = next g in
        (Func1 (fromIntegral res), g')

-- Testing Functor law on defined instances.
prop_compfunc_test testVal = fmap' id testVal == testVal  -- "id testVal" was producing an annoying error.
    where types = testVal :: Func2 (Func1 Int)

return []  -- See comments above 'quickCheckAll', in http://hackage.haskell.org/package/QuickCheck-2.8/docs/src/Test-QuickCheck-All.html
runTests = $quickCheckAll 
-- runTests = $verboseCheckAll 

main :: IO ()
main = do
    allPass <- runTests
    unless allPass exitFailure

