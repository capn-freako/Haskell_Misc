Oh, that FuncComp'd FFT!
========================

Playing with Conal's functor composition approach to FFT
(and literate Haskell).

Original author: David Banas <capn.freako@gmail.com>  
Original date:   March 23, 2017

Copyright (c) 2017 David Banas; all rights reserved World wide.

Introduction
------------

I've been struggling, for a couple years now, trying to really understand the
essence of Conal's *Functor Compostion* approach to defining the
*Fast Fourier Transform* (FFT). What follows is the trail left by my most
recent bumbling.

Conal gave a talk, on August 31, 2016, at the *Silicon Valley Haskell Meetup*
at Target Digital in Sunnyvale, in which he used some pictures, taken
from the Wikipedia article on the FFT, which helped me finally understand
what is going on. These musings are the result of me going back through
the slides for that talk, making sure I can write correct Haskell that
yields the correct numerical result.

I try, below, to write my own code, from scratch, peeking at Conal's Git
repositories only when absolutely necessary. However, because much code
(both psuedo and actual) is included in Conal's slides, there has no
doubt been some plagiarism on my part, here. I'm trusting Conal to
forgive me and believe that it was done in the spirit of learning. :)

The Discrete Fourier Transform (DFT)
------------------------------------

The *specification* for the FFT is the DFT:

$$
\\{x_n\\} <=> \\{X_k\\}
$$

$$
X_k = \sum_n{x_n e^{-i\frac{2\pi}{N}kn}} , 0 \leq k,n \lt N
$$

$$
x_n = \frac{1}{N} \sum_n{X_k e^{i\frac{2\pi}{N}kn}} , 0 \leq k,n \lt N
$$

, which transforms a discrete set of samples taken in one domain (time, space, etc.) into an equally sized set in the inverse domain (frequency, spatial frequency, etc.).

Note that, with the exception of the scaling factor and the reversal of the sign on the complex exponent, the transform is symmetric with no information loss in either direction, save for what the limited numerical precision of our computational machinery may impose.

If we assume *List* as the container functor, the Haskell expression of the DFT is a straightforward translation of the algebraic expression:

```haskell

> {-# LANGUAGE CPP
>            , TypeOperators
>            , KindSignatures
>            , TypeApplications
>            , ScopedTypeVariables
>            , AllowAmbiguousTypes
>            , TypeFamilies
>            , FlexibleContexts
>            , DeriveFunctor
>            , DeriveFoldable
>            , DeriveTraversable
>            , TemplateHaskell
>            , RankNTypes
>            , UndecidableInstances
>            , FlexibleInstances
>            -- , DeriveAnyClass
>            , GeneralizedNewtypeDeriving 
>            -- , DatatypeContexts
>   #-}
>
> {-# OPTIONS_GHC -Wall #-}
> 
> module Main where
>
> import Control.Applicative
> import Control.Monad (unless, liftM, liftM2)
> import Data.Complex
> import Data.Traversable
> import System.Exit (exitFailure)
> import Test.QuickCheck (choose, vectorOf, elements, collect)
> import Test.QuickCheck.Arbitrary
> import Test.QuickCheck.All (quickCheckAll)
> import Test.QuickCheck.Gen
> import Text.Printf
>
> dft :: [Complex Double] -> [Complex Double] 
> dft xs = [ sum [ x * exp((0.0 :+ (-1.0)) * 2 * pi / lenXs * fromIntegral(k * n)) 
>           | (x, n) <- zip xs [0..]  ] 
>           | k <- [0..(length xs - 1)]  ] 
>  where lenXs = fromIntegral $ length xs

```

**Note:** Originally, I'd intended to add LANGUAGE pragmas as needed, to
better associate them with the first code that required them. However,
that didn't work. I found that I had to include all needed pragmas in
the LANGUAGE directive, above. So, instead, I've noted, below, where a
particular pragma was first needed.

However, for this work we need something more general. We need something that can be applied to any *traversable* functor, as the data container. How can we write such a thing?

When applied to vectors, the DFT can be expressed particularly concisely using matrix notation:

$$
X = W x
$$

where *X* and *x* are column vectors containing the elements of {X<sub>k</sub>} and {x<sub>n</sub>}, respectively, and *W* is a matrix of *complex identity scalers* (CIS) with elements given by:

$$
W_{k,n} = e^{-i\frac{2\pi}{N}kn} 
$$

And Conal solves our problem, by generalizing from this matrix-vector expression:
(Note that the implied multiplication between *W* and *x*, above, is a *cross* product.)

```haskell

> type Unop a = a -> a
>
> dft' :: forall f a. ( Applicative f, Traversable f, Sized f
>                     , RealFloat a) => Unop (f (Complex a))
> dft' xs = omegas (size @f) `cross` xs
>
> cross :: (Functor n, Foldable m, Applicative m, Num a) =>
>   n (m a) -> m a -> n a  -- matrix * vector
> mat `cross` vec = dot vec <$> mat
>
> dot :: (Foldable f, Applicative f, Num a) =>
>   f a -> f a -> a        -- dot product
> u `dot` v = sum (liftA2 (*) u v)

```

He has, essentially, given us the succinct matrix-vector expression of the *FFT* operation, for any *Applicative*, *Traversable*, and *Sized* *Functor*.

We'll talk more about the *omegas* function, as well as its supporting functions, when we discuss *twiddle factor* generation, below.

Functor Composition Type
------------------------

Conal presents his new functor composition type, like this:

`newtype (g . f) a = O (g (f a))`

which doesn't compile:

    Davids-MacBook-Air-2:Haskell_Misc dbanas$ ghc -Wall func_comp_fft.lhs 
    [1 of 1] Compiling Main             ( func_comp_fft.lhs, func_comp_fft.o )
    
    func_comp_fft.lhs:9:14: error:
        Illegal symbol '.' in type
        Perhaps you intended to use RankNTypes or a similar language
        extension to enable explicit-forall syntax: forall <tvs>. <type>
    Davids-MacBook-Air-2:Haskell_Misc dbanas$ 

A quick review of the forum exchange between he and Simon Peyton Jones,
on the topic of *type operator syntax*, reveals the issue: a ':' character
must prefix all infix type operators, currently, in Haskell. Conal must
remove these, when he typesets his slides for presentation. Making this
correction yields:

```haskell

> newtype (g :. f) a = O (g (f a))
>   -- deriving (Eq, Show, Functor, Foldable, Traversable)
>   deriving (Eq, Functor, Foldable, Traversable)
>
> -- instance (Show a, Show (f a), Show (g String), Functor g) => Show ((g :. f) a) where
> instance (Show (g (f a))) => Show ((g :. f) a) where
>   show (O x) = "O (" ++ show x ++ ")"
>   -- show = (fmap . fmap) show
>
> instance (Applicative f, Applicative g) => Applicative (g :. f) where
>   -- pure x = O (pure (pure x))
>   pure = O . pure . pure
>   -- O (pure (pure h)) <*> O (pure (pure x)) = O (pure (pure (h x)))
>   -- (<*>)  = liftA2 ($)  -- Mistake, as per Conal.
>   O h <*> (O x) = O $ fmap (<*>) h <*> x
>   -- (<*>) = (<*>) . (fmap (<*>)) -- Doesn't work.

```

which compiles without error, after including the *TypeOperators*
language pragma.

**Note:** The *deriving* clause, as well as the *Applicative* instance, weren't prompted, until I introduced *assertion3*, near the end of this work. My first attempt at defining '<*>' fails with the following compiler error:

    Documents/Projects/Haskell_Misc/Conals_FFT/func_comp_fft.lhs|166 col 14 error| Parse error in pattern: pure

And I don't understand the following, re: my second attempt:

1. Why not "liftA2 . liftA2", since we have two functors?

Conal pointed out that my second attempt was wrong and (maybe) the cause of my *assertion3* causing *ghci* to hang. Querying types, at the ghci command prompt, led me to:

    *Main> :t (<*>) . (<*>)
    (<*>) . (<*>)
      :: (a1 -> a -> b) -> ((a1 -> a) -> a1) -> (a1 -> a) -> b
    *Main> :t fmap (<*>)
    fmap (<*>)
      :: (Functor f1, Applicative f) =>
         f1 (f (a -> b)) -> f1 (f a -> f b)
    *Main> :t (<*>) . (fmap (<*>))
    (<*>) . (fmap (<*>))
      :: (Applicative f, Applicative f1) =>
         f1 (f (a -> b)) -> f1 (f a) -> f1 (f b)

which led to my final solution. I don't understand why I can't express this in point free style.

Shaped Types
------------

The next thing encountered in Conal's talk (slide 13) is the *Sized* typeclass.
I was completely baffled by this, never having encountered type
operators before. I struggled briefly and then cheated, peaking at
Conal's *shaped_types* library. Copying what I found there:

```haskell

> class Sized (f :: * -> *) where
>     size :: Integer
>  
> instance (Sized f, Sized g) => Sized (g :. f) where
>     size = size @f * size @g

```

LANGUAGE pragmas introduced:

- KindSignatures
- TypeApplications
- ScopedTypeVariables
- AllowAmbiguousTypes

**Note:** I thought the *@f* syntax was the result of more
typesetting, not realizing that it was actually valid Haskell syntax,
as long as the *TypeApplications* language pragma is provided.

FFT Class
---------

Next comes Conal's definition of a class of FFT'able types (slide 14):

```haskell

> class FFT f where
>   type Reverse f :: * -> *
>   fft :: RealFloat a => f (Complex a) -> Reverse f (Complex a)

```

LANGUAGE pragmas introduced:

- TypeFamilies

**Notes:**

1. The first line of this class definition threw me at first, as this was
my first real introduction to type families. I found a very helpful
article, here: [Haskell Wiki article on *Type Families*](https://wiki.haskell.org/GHC/Type_families)

1. The necessity of *Reverse*, above, remains a "wart" (I believe, after talking w/ Conal), which Conal would like to find a way of removing.

FFT Instance for Functor Composition
------------------------------------

Conal's *FFT* instance for his functor composition type looks like this:

```haskell

> -- Constraints filled in, by responding to compiler errors incrementally.
> instance (Traversable g, Traversable (Reverse g),
>           Applicative (Reverse g),
>           FFT g,
>           Traversable f,
>           Applicative f, Applicative (Reverse f),
>           FFT f, Sized f, Sized (Reverse g) ) =>
>   FFT (g :. f) where
>     type Reverse (g :. f) = Reverse f :. Reverse g
>     fft = O . fft' . transpose . twiddle . fft' . unO
>
> fft' :: ( Traversable g
>         , Applicative (Reverse g)
>         , FFT g
>         , Traversable f
>         , Applicative f
>         , RealFloat a ) =>
>   g (f (Complex a)) -> Reverse g (f (Complex a))
> fft' = transpose . fmap fft . transpose 

```

LANGUAGE pragmas introduced:

- FlexibleContexts (to allow, "Traversable (Reverse g)", etc.)

And this definition follows very closely the pictorial diagram, which Conal has provided to help visualize the FFT computational algorithm.

Note that the *fft'* function is how we apply the FFT to the non-contiguous elements "contained" by the outer functor (i.e. - the columns in the picture).
This is achieved by first transposing, making the outer functor the inner functor, and then *mapping* the fft' operator over the new outer functor (i.e. - *applying* it to each new inner (old outer) functor, in turn).

Now, you might ask, "Why transpose then map? Why not just apply?"
Well, because then we wouldn't be breaking down the computation with each recursive step. We'd essentially be saying:

```haskell
fft (g (f a)) = fft (g (f a))
```

which isn't very helpful. ;-)

Generating the Twiddle Factors
------------------------------

The *twiddle* function is implemented in a particularly elegant manner:

```haskell

> twiddle :: forall g f a. ( Applicative g, Traversable g, Sized g 
>                          , Applicative f, Traversable f, Sized f
>                          , RealFloat a ) =>
>   g (f (Complex a)) -> g (f (Complex a))
> -- twiddle = (liftA2 . liftA2) (*) (omegas (size @(g :.: f)))
> twiddle = (liftA2 . liftA2) (*) (omegas (size @(g :. f)))
>
> omegas :: ( Applicative g, Traversable g
>           , Applicative f, Traversable f
>           , RealFloat a) =>
>   Integer -> g (f (Complex a))
> -- 'i' was unrecognized.
> -- omegas n = powers <$> powers (exp (-i * 2 * pi / fromIntegral n))
> omegas n = powers <$> powers (cis (2 * pi / fromIntegral n))
>
> powers :: ( Applicative f, Traversable f
>           , Fractional a) => a -> f a
> -- I don't have time to digest Conal's "Generic Parallel Scan" talk, right now.
> -- powers = fst . lscanAla Product . pure
> -- But, I do need to accomodate all applicative functors, as opposed to just lists.
> -- So, I'll try the Traversable approach.
> powers w = fmap (/ w) . snd . mapAccumL (\x y -> (x * y, x * y)) 1 $ pure w

```

The work horse, *powers*, uses *pure* and *mapAccumL*, in order to generalize away from lists to any *Applicative* *Traversable* functor. The *omegas* function then makes use of this functionality, via *fmap* (i.e. - <$>), to create the functor composition equivalent of the matrix of twiddle factors, which would be required if we were using a more conventional vector/matrix approach to the computation. Finally, *twiddle* uses the double *liftA2* trick to multiply the incoming functor composition by this twiddle "matrix".

The coolest part about this approach, IMHO, is that the *Sized f* constraint never drops below *twiddle* in the function call stack. And this keeps the lower functions in that stack (i.e. - *omegas* and *powers*) more general than they would be otherwise. This is a really clever use of *Applicative Functor* (i.e. - *pure* is the enabling ingredient.) and is what, finally, helped me grok its real power.

**Note:** There's something odd here: I was forced to use explicit *forall* syntax, in the type signature for *twiddle*, in order to be allowed use of the *@&lt;type variable&gt;* syntax in its definition, via the *ScopedTypeVariables* language pragma. However, I did not have to do this, in the *Sized* instance for the functor composition type, above. (See the *Shaped Types* section, above.) I don't understand why the two are different. Is it because, here, I'm applying '@' to a type *constructor*, as opposed to a type *variable*?

Miscellaneous Stuff
-------------------

```haskell

> unO :: (g :. f) a -> g (f a)
> unO (O x) = x
>
> -- Copied from Conal's generic-fft library.
> transpose :: (Traversable g, Applicative f) => g (f a) -> f (g a)
> transpose = sequenceA

```

Types for Testing
-----------------

```haskell

> -- Quad
> newtype Quad a = Quad ((a,a),(a,a))
>   deriving (Show, Functor, Foldable, Traversable)
>
> instance Applicative Quad where
>   pure x = Quad ((x,x),(x,x))
>   Quad ((g,h),(u,v)) <*> Quad ((w,x),(y,z)) = Quad ((g w, h x),(u y, v z))
>
> instance Sized Quad where
>   size = 4
>
> -- Pair
> data  Pair a = a :# a
>   deriving ( Show, Eq, Functor, Foldable, Traversable )
>
> instance Applicative Pair where
>   pure x = x :# x
>   g :# h <*> (x :# y) = g x :# h y
>
> instance Sized Pair where
>   size = 2
>
> instance FFT Pair where
>   type Reverse Pair = Pair
>   fft (x :# y) = (x + y) :# (x - y)
>
> -- Tree of Pairs
> data Tree a = Leaf (Pair a)
>             | Branch (Pair (Tree a))
>   deriving (Show, Eq, Functor, Foldable, Traversable)
>
> instance Applicative Tree where
>   -- pure x = Leaf (x :# x)
>   pure                                        = Leaf . pure
>   Leaf   p1           <*> Leaf   p2           = Leaf   $ p1 <*> p2
>   Branch (tg1 :# tg2) <*> Branch (tx1 :# tx2) = Branch $ (tg1 <*> tx1) :# (tg2 <*> tx2)

```

**Note:** The *Sized* and *FFT* instances are going to take a lot more work, after talking to Conal.

```haskell

> {-
> instance Sized Tree where
>   size (Leaf   _)          = 2
>   -- size (Branch (t1 :# t2)) = size t1 + size t2
>   size (Branch (t1 :# t2)) = size O (t1 :# t2)
>
> instance FFT Tree where
>   type Reverse Tree = Tree
>   fft (Leaf p)   = Leaf   (fft p)
>   fft (Branch p) = Branch (transpose $ unO $ fft (O p))
>
> -- Tree building utility.
> mkTree :: Integer -> [a] -> Tree a
> mkTree n xs | n == 0    = Leaf (head xs :# head (tail xs))
>             | otherwise = Branch (mkTree (n-1) (evens xs) :# mkTree (n-1) (odds xs))
> -}

```

**Note:** Trying to implement the *Sized* instance for *Tree* made me realize that I need the *size* function to take an argument. How does Conal get away with an argument-free version of *size*, while still creating an *FFT* instance for top-down trees?

Tree Repackaging
----------------

**Note:** Before realizing that instancing *FFT* for *Tree* was, probably, a better path, I was attempting to repackage a *Tree* into a *Functor composition*. I spent a fair amount of effort, and generated a large amount of code, persuing this. I wanted to keep this code as a record of the various paths I travelled. But, rather than clog this section with the remnance of that abandoned attempt, I've put it in an appendix, near the end of this document.

**Note:** Before giving up on converting *Tree*s to functor compositions, I learned a lot about universally/existentially quantified types. I'd like to thank Conal and John for their helpful guidance, as I struggled to grok all the subtleties of this topic. Also, some links I found, which were useful:
1. [Existential vs. Universally Quantified Types in Haskell](http://stackoverflow.com/questions/14299638/existential-vs-universally-quantified-types-in-haskell)
2. [John Wiegley's quantification talk slides](https://github.com/jwiegley/haskell-quantification)

Primitive Testing
-----------------

Before switching to *QuickCheck*, I just wrote a few simple assertions to ensure my code was producing the correct numerical answers to some *very* simple test cases:

```haskell

> toList :: Foldable f => f a -> [a]
> toList = foldr (:) []
>
> assertion1 :: Bool
> assertion1 = dft (map (:+ 0.0) [1.0, 0.0, 0.0, 0.0]) == [1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0, 1.0 :+ 0.0]
>
> assertion2 :: Bool
> assertion2 = dft (map (:+ 0.0) [1.0, 0.0, 0.0, 0.0]) == toList (dft' (fmap (:+ 0.0) (Quad ((1.0, 0.0), (0.0, 0.0)))))
>
> assertion3 :: Bool
> assertion3 = fft (fmap (:+ (0 :: Double)) (O ((1 :# 0) :# (0 :# 0)))) == dft' (fmap (:+ (0::Double)) (O ((1 :# 0) :# (0 :# 0))))
>
> exeMain :: IO ()
> exeMain = do
>   print (powers (cis (2 * pi / 4)) :: Quad (Complex Float))
>   print assertion1
>   print assertion2
>   print assertion3

```

**Note:** In my first attempt at defining *assertion3*, above, I forgot to use the *O* constructor to form a functor composition, which resulted in this compiler error:

    Documents/Projects/Haskell_Misc/Conals_FFT/func_comp_fft.lhs|362 col 16 error| • No instance for (RealFloat (Pair a0)) arising from a use of ‘fft’
    || • In the first argument of ‘(==)’, namely
    ||     ‘fft (fmap (:+ 0.0) ((1.0 :# 0.0) :# (0.0 :# 0.0)))’
    ||   In the expression:
    ||     fft (fmap (:+ 0.0) ((1.0 :# 0.0) :# (0.0 :# 0.0)))
    ||     == dft' (fmap (:+ 0.0) ((1.0 :# 0.0) :# (0.0 :# 0.0)))
    ||   In an equation for ‘assertion3’:
    ||       assertion3
    ||         = fft (fmap (:+ 0.0) ((1.0 :# 0.0) :# (0.0 :# 0.0)))
    ||           == dft' (fmap (:+ 0.0) ((1.0 :# 0.0) :# (0.0 :# 0.0)))

And this led me on a wild goose chase, trying to figure out how to get the *RealFloat* instance derived automatically for the *Pair* type, which of course was never really necessary.

**Questions**
1. Why don't I need "fmap . fmap", in the definition of *assertion3*?

QuickCheck definitions.
-----------------------

**Note:** I did a whole bunch of stuff that was futile, without the use of *unsafeCoerce*, and have moved that into **Appendix B**, for the historical record.

```haskell

> -- QuickCheck types & propositions
> {-|
> A custom double precision floating point type, which overides the 'Show'
> and 'Eq' instances, in order to:
> 
>     * Limit precision to 3 places after the decimal.
> 
>     * Print ridiculously small numbers as simply zero.
> 
>     * Define equality as difference being less than some threshold.
> -}
> newtype PrettyDouble = PrettyDouble {
>     uglyDouble :: Double
>   } deriving (Ord, Fractional, Floating, Real, RealFrac, RealFloat, Enum, Arbitrary, CoArbitrary)
> 
> instance Show PrettyDouble where
>     show = printf "%6.3g" . zeroThresh . uglyDouble
>         where zeroThresh y =
>                 if abs y < 1.0e-10
>                 then 0.0
>                 else y
> 
> instance Eq PrettyDouble where
>     z1' == z2' = abs (z1 - z2) < 1.0e-3
>         where z1 = uglyDouble z1'
>               z2 = uglyDouble z2'
>               
> instance Num PrettyDouble where
>     x + y         = PrettyDouble $ uglyDouble x + uglyDouble y
>     x - y         = PrettyDouble $ uglyDouble x - uglyDouble y
>     x * y         = PrettyDouble $ uglyDouble x * uglyDouble y
>     abs x         = PrettyDouble $ abs $ uglyDouble x
>     signum x      = PrettyDouble $ signum $ uglyDouble x
>     fromInteger n = PrettyDouble $ fromIntegral n
> 
> newtype FFTTestVal f g = FFTTestVal {
>   getVal :: (g :. f) (Complex PrettyDouble)
> }
>
> instance (Show (g (f (Complex PrettyDouble)))) => Show (FFTTestVal f g) where
>     show x = "FFTTestVal (" ++ (show . getVal) x ++ ")"
>
> instance Arbitrary (FFTTestVal Pair Pair) where
>   arbitrary = do vals <- vectorOf 4 (choose (-1.0, 1.0))
>                  let vals' = map ((:+ 0.0) . PrettyDouble) vals
>                  let res   = pOfP vals'
>                  return $ FFTTestVal $ O res
>
> instance Arbitrary (FFTTestVal (Pair :. Pair) Pair) where
>   arbitrary = do vals <- vectorOf 8 (choose (-1.0, 1.0))
>                  let vals' = map ((:+ 0.0) . PrettyDouble) vals
>                  let res   = O (pOfP (take 4 vals')) :# O (pOfP (drop 4 vals'))
>                  return $ FFTTestVal $ O res
>
> pOfP :: [a] -> Pair (Pair a)
> pOfP xs = (x0 :# x1) :# (x2 :# x3)
>   where (x0, xs')   = (head xs,   tail xs)
>         (x1, xs'')  = (head xs',  tail xs')
>         (x2, xs''') = (head xs'', tail xs'')
>         x3          = head xs'''
>
> -- prop_fail = False  -- Test that QuickCheck is, actually, running.
>
> prop_fft_test1 testVal = toList fft_res == toList dft_res
>   where types     = testVal :: FFTTestVal Pair Pair
>         val       = getVal testVal
>         dft_res   = dft' val
>         fft_res   = fft  val
>
> prop_fft_test2 testVal = toList fft_res == toList dft_res
>   where types     = testVal :: FFTTestVal (Pair :. Pair) Pair
>         val       = getVal testVal
>         dft_res   = dft' val
>         fft_res   = fft  val
>
> -- Entry point for unit tests.
> return []
> runTests = $quickCheckAll
> 
> testMain :: IO ()
> testMain = do
>     allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
>     unless allPass exitFailure
>     putStrLn "Success!"
> 
> -- This is a clunky, but portable, way to use the same Main module file
> -- for both an application and for unit tests.
> -- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
> -- That way we can use the same file for both an application and for tests.
> -- #ifndef MAIN_FUNCTION
> -- #define MAIN_FUNCTION exeMain
> -- #endif
> -- main = MAIN_FUNCTION
> main :: IO ()
> main = testMain
> -- main = exeMain

```

LANGUAGE pragmas introduced:

- CPP
- TemplateHaskell
- RankNTypes
- UndecidableInstances (for the Show instance for FFTTestVal)
- FlexibleInstances (to allow "instance Arbitrary (FFTTestVal f *Pair*)...
- GeneralizedNewtypeDeriving (for *Fractional* instance for *PrettyDouble*.)

Test Results
------------

=== prop_fft_test1 from func_comp_fft.lhs:565 ===
+++ OK, passed 100 tests.

=== prop_fft_test2 from func_comp_fft.lhs:571 ===
+++ OK, passed 100 tests.

Success!

Appendix A - Code left over from my abandoned attempt at converting *Tree*s to functor compositions
---------------------------------------------------------------------------------------------------

```haskell

> {-
>
> newtype Id a = Id {unId :: a}
>   deriving ( Show, Eq, Functor, Foldable, Traversable )
>
> instance Applicative Id where
>   pure = Id
>   Id g <*> (Id x) = Id (g x)
>
> instance Sized Id where
>   size = 1
>
> instance FFT Id where
>   type Reverse Id = Id
>   fft (Id x) = Id x
>
> -- Converting trees to functor compositions, for FFT'ing.
> tree2FuncComp :: Tree a -> (g :. f) a
> tree2FuncComp (Leaf (x :# y))     = O (Id x :# Id y)
> tree2FuncComp (Branch (t1 :# t2)) = O (tree2FuncComp t1 :# tree2FuncComp t2)
>
> -- simple Functor test (Didn't work.)
> repackTest :: (forall a. a -> r) -> Tree b -> r
> repackTest f (Leaf p)   = f p
> repackTest f (Branch p) = f p
>
> tree2list :: Tree a -> [Pair a]
> tree2list = repackTest (: [])
>
> -- Applicative test (Didn't work.)
> repackTest :: Applicative r => Tree b -> r a
> repackTest (Leaf p)   = pure p
> repackTest (Branch p) = pure p
>
> -- repackageTree :: (forall a f g. g (f a) -> r b) -> Tree c -> r b
> repackageTree :: (forall a f g. g (f a) -> r) -> Tree c -> r
> -- repackageTree _ (Leaf _)                        = error "Can't repackage a Leaf!"
> repackageTree f (Branch (Leaf pr1 :# Leaf pr2)) = f (pr1 :# pr2)
> repackageTree f (Branch (t1 :# t2))             = f (repackageTree f t1 :# repackageTree f t2)
>
> -- treeToO :: Tree a -> (g :. f) a
> -- treeToO = repackageTree O
>
> -- mkPairs :: Integer -> [a] -> Pair (Pair a)  -- This won't work; it fixes the depth at 2.
> mkPairs n xs = unId $ traverse (Id . unwrap) $ mkTree n xs
> -- mkPairs = unId . traverse (Id . unwrap) . mkTree  -- Not sure why this didn't work.
>
> unwrap :: Tree a -> a
> unwrap (Leaf x) = x
> unwrap (Branch (t1 :# t2)) = unwrap t1 :# unwrap t2
>
> -- mkPofP :: [a] -> (g :. f) a
> -- mkPofP :: [a] -> Pair (Pair b)
> mkPofP :: [a] -> Pair (Pair a)
> -- mkPofP []        = error "Empty list provided!"
> -- mkPofP [w]       = error "Wrong number of elements given!"
> -- mkPofP [w,x]     = error "Wrong number of elements given!"
> -- mkPofP [w,x,y]   = error "Wrong number of elements given!"
> -- mkPofP [w,x,y,z] = O ((w :# x) :# (y :# z))
> -- mkPofP xs        = O (unO (mkPofP (evens xs)) :# (unO (mkPofP (odds xs))))
> mkPofP [w,x,y,z] = (w :# y) :# (x :# z)
> -- mkPofP xs        = mkPofP (evens xs) :# mkPofP (odds xs)
> mkPofP _         = error "Wrong number of elements given!"
>
> mkP :: Integer -> Pair a
> mkP n | n == 0    = choose (-1.0, 1.0) :# choose (-1.0, 1.0)
>       | otherwise = mkP (n-1) :# mkP (n-1)
>
> evens :: [a] -> [a]
> evens []     = []
> evens (x:xs) = x : odds xs
>
> odds :: [a] -> [a]
> odds []     = []
> odds (_:xs) = evens xs
>
> -}

```

Appendix B - Code left over from my abandoned attempt at using recursive QuickCheck machinery, for test data generation.
------------------------------------------------------------------------------------------------------------------------

Had I understood the type theoretical underpinnings of Haskell better, I'd have known that this was futile, since I *have* to use a sum type, to build a data structure, the arbitrary depth of which is determined only at run time, and there is no *FFT* instance for sum types, yet.

Thanks to both Conal and John for pointing out the following, in this regard:
1. Sum types are necessary, in order to build arbitrary depth data structures at run time. (both)
2. There is no *FFT* instance for sum types, yet. (Conal)
3. The only way to "hide" the fact that sum types were used to build your data structure is *unsafeCoerce*. (John)

```haskell

> {-
> -- (:#)               :: a -> a -> Pair a
> -- O                  :: g (f a) -> (:.) g f a
> -- O (:#)             :: (:.) ((->) a) ((->) a) (Pair a)
> -- O . (:#)           :: a -> (:.) ((->) a) Pair a
> -- O (uncurry (:#))   :: (:.) ((->) (a, a)) Pair a
> -- O . (uncurry (:#)) :: (f a, f a) -> (:.) Pair f a
> 
> instance Functor f => Arbitrary (FFTTestVal f Pair) where
>   -- arbitrary = return $ FFTTestVal $ O (pure (pure (1 :+ (0.0::Double))))
>   arbitrary = liftM FFTTestVal tree
>
> tree :: Functor f => Gen ((Pair :. f) a)
> tree = sized tree'
> -- tree' 0       = liftM id arbitrary
> tree' 0       = liftM2 (:#) arbitrary arbitrary
> -- tree' 0       = fmap id arbitrary
> -- tree' 0       = id arbitrary
> -- tree' 0       = arbitrary
> -- tree' n | n>0 = fmap (O . uncurry (:#)) (fmap (subtree, subtree))
> tree' n | n>0 = fmap (O . uncurry (:#)) (liftM2 (,) subtree subtree)
>   where subtree = tree' (n - 1)
> -}
 
```

