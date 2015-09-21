-- Typeclassopedia - Sec. 3.2, Exercise 3.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   September 20, 2015
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

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

instance Eq a => Eq (ITree a) where
    Leaf g == Leaf h = g 0 == h 0
    Node xs == Node ys = and (zipWith (==) xs ys)
    Leaf _ == Node _ = False
    Node _ == Leaf _ = False

instance Show (ITree a) where
    show _ = "No representation, yet."

-- Functor ITree
instance MyFunctor ITree where
    fmap' g (Leaf f)   = Leaf (g . f)
    fmap' g (Node its) = Node $ map (fmap' g) its

instance (Num a, Random a) => Arbitrary (ITree a) where
    arbitrary = return $ Node [Leaf fromIntegral, Node [Leaf fromIntegral, Leaf fromIntegral]]

-- Testing Functor law on defined instances.
prop_itree_test testVal = fmap' id testVal == testVal  -- "id testVal" was producing an annoying error.
    where types = testVal :: ITree Int

return []  -- See comments above 'quickCheckAll', in http://hackage.haskell.org/package/QuickCheck-2.8/docs/src/Test-QuickCheck-All.html
runTests = $quickCheckAll 
-- runTests = $verboseCheckAll 

main :: IO ()
main = do
    allPass <- runTests
    unless allPass exitFailure

