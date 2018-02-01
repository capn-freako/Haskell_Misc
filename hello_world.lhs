Oh, that FuncComp'd FFT!
========================

Playing with Conal's functor composition approach to FFT
(and literate Haskell).

Original author: David Banas <capn.freako@gmail.com>  
Original date:   March 23, 2017

Copyright (c) 2017 David Banas; all rights reserved World wide.

Introduction
------------

I've been struggling, for a couple years now, to really understand the
essence of Conal's *Functor Compostion* approach to defining the
*Fast Fourier Transform* (FFT). What follows is the trail left by my most
recent bumbling.

Conal gave a talk, on August 31, 2016, at the *Bay Area Haskell Meetup*
at Target Digital in Sunnyvale, in which he used some pictures, taken
from the Wikipedia article on the FFT, which helped me finally understand
what is going on. These musings are the result of me going back through
the slides for that talk, making sure I can write correct Haskell that
yieldds the correct numerical result.

I try, below, to write my own code, from scratch, peeking at Conal's Git
repositories only when absolutely necessary. However, because much code
(both psuedo and actual) is included in Conal's slides, there has no
doubt been some plagiarism on my part, here. I'm trusting Conal to
forgive me and believe that it was done in the spirit of learning. :)

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

> {-# LANGUAGE TypeOperators
>            , KindSignatures
>            , TypeApplications
>            , ScopedTypeVariables
>            , AllowAmbiguousTypes
>            , TypeFamilies
>   #-}
>
> {-# OPTIONS_GHC -Wall #-}
> 
> module Dummy where
>
> import Data.Complex
> import Control.Applicative
> import Data.Traversable
>
> newtype (g :.: f) a = O (g (f a))

```

which compiles without error, after including the *TypeOperators*
language pragma.

**Note:** Originally, I'd intended to add LANGUAGE pragmas as needed, to
better associate them with the first code that required them. However,
that didn't work. I found that I had to include all needed pragmas in
the LANGUAGE directive, above. So, instead, I've noted, below, where a
particular pragma was first needed.

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
> instance (Sized f, Sized g) => Sized (g :.: f) where
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
>   fft :: f (Complex a) -> Reverse f (Complex a)

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

> main = putStrLn "Hello, World!"

> unO :: (g :.: f) a -> g (f a)
> unO (O x) = x
>
> -- Copied from Conal's generic-fft library.
> transpose :: (Traversable g, Applicative f) => g (f a) -> f (g a)
> transpose = sequenceA
>
> instance (Traversable g, Applicative f) => FFT (g :.: f) where
>   -- type Reverse (g :.: f) = Reverse f :.: Reverse g
>   type Reverse (g :.: f) = f :.: g
>   fft = O . fft' . transpose . twiddle . fft' . unO
>
> fft' :: g (f (Complex a)) -> Reverse g (f (Complex a))
> fft' = transpose . fmap fft . transpose 
>
> twiddle :: forall g f a. g (f (Complex a)) -> g (f (Complex a))
> twiddle = (liftA2 . liftA2) (*) (omegas (size @(g :.: f)))

```

And this definition follows very closely the pictorial diagram, which Conal has provided to help visualize the FFT computational algorithm.

Note that the *fft'* function is how we apply the FFT to the non-contiguous elements "contained" by the outer functor (i.e. - the columns in the picture).
This is achieved by first transposing, making the outer functor the inner functor, and then mapping the fft' operator over the new outer functor (i.e. - applying it to each new inner (old outer) functor, in turn).

Now, you might ask, "Why transpose then map? Why not just apply?"
Well, because then we wouldn't be breaking down the computation with each recursive step. We'd essentially be saying:

```haskell
fft (g (f a)) = fft (g (f a))
```

which isn't very helpful. ;-)

Note the revised *type* line in the instance declaration, above. It was necessary,
in order to avoid this type error:

    func_comp_fft.lhs|159 col 11 error| • Couldn't match type ‘g’ with ‘Reverse g’

And it imposes a rather severe restriction: the instance will only function
correctly for compositions of *primitive* functors, not for compositions of
*composite* functors. I need to talk to Conal about how he gets around this.
There must be a way to define *id* as the default implementation of *Reverse*,
but I don't know what it is.

Generating the Twiddle Factors
------------------------------

The *twiddle* function is implemented in a particularly elegant manner:


```haskell

> main :: IO ()
> main = do
>   putStrLn "Success!"

```

