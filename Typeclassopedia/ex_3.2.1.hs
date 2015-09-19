-- Typeclassopedia
--
-- 3.2 Exercises

{-# LANGUAGE TemplateHaskell #-}

module Main where

-- import Test.QuickCheck
import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.QuickCheck (choose, vectorOf, elements, collect)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All (quickCheckAll, verboseCheckAll)

class MyFunctor f where
    fmap' :: (a -> b) -> f a -> f b

-- Functor (Either e)
instance MyFunctor (Either e) where
    fmap' g (Left e)  = Left e
    fmap' g (Right x) = Right (g x)

-- Functor ((->) e)
instance MyFunctor ((->) e) where
    fmap' g f = g . f

-- Testing Functor law on defined instances.
prop_either_test testVal = fmap' id testVal == testVal  -- "id testVal" was producing an annoying error.
    where types = testVal :: (Either String Int)

prop_function_test testVal = fmap' id testFunc testVal == testFunc testVal
    where testFunc :: Int -> Int
          testFunc x = 2 * x
          types = testVal :: Int

return []  -- See comments above 'quickCheckAll', in http://hackage.haskell.org/package/QuickCheck-2.8/docs/src/Test-QuickCheck-All.html
runTests = $quickCheckAll 
-- runTests = $verboseCheckAll 

main :: IO ()
main = do
    allPass <- runTests
    unless allPass exitFailure

