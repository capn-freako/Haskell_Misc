-- Typeclassopedia - Sec. 3.3, Exercise 1.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   September 22, 2015
--
-- Copyright (c) 2015 David Banas; all rights reserved World wide.

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.Random (Random)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)

class MyFunctor f where
    fmap' :: (a -> b) -> f a -> f b

instance MyFunctor [] where
    fmap' _ []     = []
    fmap' g (x:xs) = (g x, g x) : fmap' g xs

-- Testing Functor law on defined instances.
prop_id_test :: [Int] -> Bool
prop_id_test testVal = fmap' id testVal == testVal  -- "id testVal" was producing an annoying error.
    where types = testVal :: [Int]

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

prop_comp_test :: [Int] -> Bool
prop_comp_test testVal = fmap' (snd . id) testVal == (fmap' (1 +) . fmap' (1 +)) testVal
    where types = testVal :: [Int]

return []  -- See comments above 'quickCheckAll', in http://hackage.haskell.org/package/QuickCheck-2.8/docs/src/Test-QuickCheck-All.html
runTests :: IO Bool
runTests = $quickCheckAll 
-- runTests = $verboseCheckAll 

main :: IO ()
main = do
    allPass <- runTests
    unless allPass exitFailure

