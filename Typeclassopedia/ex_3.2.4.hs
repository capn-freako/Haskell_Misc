-- Typeclassopedia - Sec. 3.2, Exercise 4.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   September 21, 2015
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

-- Can't be made a functor, due to 'Int' type restriction.
data NoFunc Int = NoFunc Int

