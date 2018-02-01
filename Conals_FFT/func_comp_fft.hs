{-# LANGUAGE CPP
           , TypeOperators
           , KindSignatures
           , TypeApplications
           , ScopedTypeVariables
           , AllowAmbiguousTypes
           , TypeFamilies
           , FlexibleContexts
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , TemplateHaskell
           , RankNTypes
           , UndecidableInstances
           -- , DeriveAnyClass
           -- , GeneralizedNewtypeDeriving 
           -- , DatatypeContexts
  #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative
import Control.Monad (unless)
import Data.Complex
import Data.Traversable
import System.Exit (exitFailure)
import Test.QuickCheck (choose, vectorOf)  -- , elements, collect)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.All (quickCheckAll)

dft :: [Complex Double] -> [Complex Double] 
dft xs = [ sum [ x * exp((0.0 :+ (-1.0)) * 2 * pi / lenXs * fromIntegral(k * n)) 
          | (x, n) <- zip xs [0..]  ] 
          | k <- [0..(length xs - 1)]  ] 
 where lenXs = fromIntegral $ length xs

type Unop a = a -> a

dft' :: forall f a. ( Applicative f, Traversable f, Sized f
                    , RealFloat a) => Unop (f (Complex a))
dft' xs = omegas (size @f) `cross` xs

cross :: (Functor n, Foldable m, Applicative m, Num a) =>
  n (m a) -> m a -> n a  -- matrix * vector
mat `cross` vec = dot vec <$> mat

dot :: (Foldable f, Applicative f, Num a) =>
  f a -> f a -> a        -- dot product
u `dot` v = sum (liftA2 (*) u v)

newtype (g :. f) a = O (g (f a))
  deriving (Eq, Functor, Foldable, Traversable)

instance (Show a, Show (f a), Show (g String), Functor g) => Show ((g :. f) a) where
  show (O x) = show $ fmap show x

instance (Applicative f, Applicative g) => Applicative (g :. f) where
  pure = O . pure . pure
  O h <*> (O x) = O $ fmap (<*>) h <*> x

class Sized (f :: * -> *) where
    size :: Integer
 
instance (Sized f, Sized g) => Sized (g :. f) where
    size = size @f * size @g

class FFT f where
  type Reverse f :: * -> *
  fft :: RealFloat a => f (Complex a) -> Reverse f (Complex a)

-- Constraints filled in, by responding to compiler errors incrementally.
instance (Traversable g, Traversable (Reverse g),
          Applicative (Reverse g),
          FFT g,
          Traversable f,
          Applicative f, Applicative (Reverse f),
          FFT f, Sized f, Sized (Reverse g) ) =>
  FFT (g :. f) where
    type Reverse (g :. f) = Reverse f :. Reverse g
    fft = O . fft' . transpose . twiddle . fft' . unO
fft' :: ( Traversable g
        , Applicative (Reverse g)
        , FFT g
        , Traversable f
        , Applicative f
        , RealFloat a ) =>
  g (f (Complex a)) -> Reverse g (f (Complex a))
fft' = transpose . fmap fft . transpose 

twiddle :: forall g f a. ( Applicative g, Traversable g, Sized g 
                         , Applicative f, Traversable f, Sized f
                         , RealFloat a ) =>
  g (f (Complex a)) -> g (f (Complex a))
twiddle = (liftA2 . liftA2) (*) (omegas (size @(g :. f)))

omegas :: ( Applicative g, Traversable g
          , Applicative f, Traversable f
          , RealFloat a) =>
  Integer -> g (f (Complex a))
-- 'i' was unrecognized.
-- omegas n = powers <$> powers (exp (-i * 2 * pi / fromIntegral n))
omegas n = powers <$> powers (cis (2 * pi / fromIntegral n))

powers :: ( Applicative f, Traversable f
          , Fractional a) => a -> f a
-- I don't have time to digest Conal's "Generic Parallel Scan" talk, right now.
-- powers = fst . lscanAla Product . pure
-- But, I do need to accomodate all applicative functors, as opposed to just lists.
-- So, I'll try the Traversable approach.
powers w = fmap (/ w) . snd . mapAccumL (\x y -> (x * y, x * y)) 1 $ pure w

unO :: (g :. f) a -> g (f a)
unO (O x) = x

-- Copied from Conal's generic-fft library.
transpose :: (Traversable g, Applicative f) => g (f a) -> f (g a)
transpose = sequenceA

-- Quad
newtype Quad a = Quad ((a,a),(a,a))
  deriving (Show, Functor, Foldable, Traversable)
instance Applicative Quad where
  pure x = Quad ((x,x),(x,x))
  Quad ((g,h),(u,v)) <*> Quad ((w,x),(y,z)) = Quad ((g w, h x),(u y, v z))
instance Sized Quad where
  size = 4

-- Pair
data  Pair a = a :# a
  deriving ( Show, Eq, Functor, Foldable, Traversable )
instance Applicative Pair where
  pure x = x :# x
  g :# h <*> (x :# y) = g x :# h y
instance Sized Pair where
  size = 2
instance FFT Pair where
  type Reverse Pair = Pair
  fft (x :# y) = (x + y) :# (x - y)

-- Tree of Pairs
data Tree a = Leaf (Pair a)
            | Branch (Pair (Tree a))
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- Tree building utility.
mkTree :: Integer -> [a] -> Tree a
mkTree n xs | n == 0    = Leaf (head xs :# head (tail xs))
            | otherwise = Branch (mkTree (n-1) (evens xs) :# mkTree (n-1) (odds xs))

-- Pair of Pair building utility.
mkPofP :: [a] -> Pair (Pair a)
-- mkPofP []        = error "Empty list provided!"
-- mkPofP [w]       = error "Wrong number of elements given!"
-- mkPofP [w,x]     = error "Wrong number of elements given!"
-- mkPofP [w,x,y]   = error "Wrong number of elements given!"
-- mkPofP [w,x,y,z] = O ((w :# x) :# (y :# z))
-- mkPofP xs        = O (unO (mkPofP (evens xs)) :# (unO (mkPofP (odds xs))))
mkPofP [w,x,y,z] = (w :# y) :# (x :# z)
-- mkPofP xs        = mkPofP (evens xs) :# mkPofP (odds xs)
mkPofP _         = error "Wrong number of elements given!"

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs

newtype Id a = Id {unId :: a}
  deriving ( Show, Eq, Functor, Foldable, Traversable )

instance Applicative Id where
  pure = Id
  Id g <*> (Id x) = Id (g x)

instance Sized Id where
  size = 1

instance FFT Id where
  type Reverse Id = Id
  fft (Id x) = Id x

toList :: Foldable f => f a -> [a]
toList = foldr (:) []

assertion1 :: Bool
assertion1 = dft (map (:+ 0.0) [1.0, 0.0, 0.0, 0.0]) == [1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0]

assertion2 :: Bool
assertion2 = dft (map (:+ 0.0) [1.0, 0.0, 0.0, 0.0]) == toList (dft' (fmap (:+ 0.0) (Quad ((1.0, 0.0), (0.0, 0.0)))))

assertion3 :: Bool
assertion3 = fft (fmap (:+ (0 :: Double)) (O ((1 :# 0) :# (0 :# 0)))) == dft' (fmap (:+ (0::Double)) (O ((1 :# 0) :# (0 :# 0))))

exeMain :: IO ()
exeMain = do
  print (powers (cis (2 * pi / 4)) :: Quad (Complex Float))
  print assertion1
  print assertion2
  print assertion3

-- QuickCheck types & propositions
newtype FFTTestVal  f g = FFTTestVal {
  getVal :: (g :. f) (Complex Double)
} -- deriving (Show)

instance (Show (f (Complex Double)), Show (g String), Functor g) =>
  Show (FFTTestVal f g) where
    show = show . getVal

instance (Applicative f, Applicative g) => Arbitrary (FFTTestVal f g) where
    arbitrary = do
        depth  <- choose (2, 5)
        -- rvals <- vectorOf (2^depth) (choose (-1.0, 1.0))
        -- ivals <- vectorOf (2^depth) (choose (-1.0, 1.0))
        -- let values = zipWith (:+) rvals ivals
        -- return $ FFTTestVal (fmap (:+ 0.0) (mkP depth))
        -- return $ FFTTestVal (tree2FuncComp $ mkTree depth values)
        return $ FFTTestVal $ O (pure (pure (depth :+ (0.0::Double))))

-- prop_fail = False  -- Test that QuickCheck is, actually, running.

prop_fft_test testVal = fft val == dft' val
  where types = testVal :: FFTTestVal Pair Pair
        val   = getVal testVal

-- Entry point for unit tests.
return []
runTests = $quickCheckAll

testMain :: IO ()
testMain = do
    -- allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    allPass <- runTests
    unless allPass exitFailure
    putStrLn "Success!"

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
-- #ifndef MAIN_FUNCTION
-- #define MAIN_FUNCTION exeMain
-- #endif
main :: IO ()
-- main = MAIN_FUNCTION
main = testMain

