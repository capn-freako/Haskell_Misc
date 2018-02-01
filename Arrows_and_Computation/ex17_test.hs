{-# LANGUAGE Arrows #-}

import Control.Arrow
import Data.Complex

-- The DFT forms the specification for the FFT.
dft :: RealFloat a => [Complex a] -> [Complex a]
dft xs = [ sum [ x * exp((0.0 :+ (-1.0)) * 2 * pi / lenXs * fromIntegral(k * n))
                 | (x, n) <- zip xs [0..]
               ]
           | k <- [0..(length xs - 1)]
         ]
    where lenXs = fromIntegral $ length xs

-- FFT implementation
-- Note my reversal of Ross' even/odd convention, below.
-- I did this, to remain consistent with the explanation, above, which came from a pre-existing work.
fft :: RealFloat b => Hom (Complex b) (Complex b)
fft = id :&: proc (e, o) -> do
                e' <- fft             -< e
                o' <- fft >>> twiddle -< o
                unriffle -< f (e', o')
    where f (x, y) = (x + y, x - y)

twiddle :: RealFloat b => Hom (Complex b) (Complex b)
twiddle = id :&: twiddle' 1

twiddle' :: RealFloat b => Int -> Hom (Pair (Complex b)) (Pair (Complex b))
twiddle' n = (id *** (* phi)) :&: ((twiddle' n') *** ((twiddle' n') >>> (arr ((* phi) `prod` (* phi)))))
    where phi = cis (-pi / (fromIntegral n))
          n'  = n + 1

-- Testing machinery
-- Conversion to list, for comparison to specification (i.e. - DFT)
btToList :: BalTree b -> [b]
btToList (Zero x)      = [x]
btToList (Succ (x, y)) = btToList x ++ (btToList y)

-- Build a tree of depth 'n', taking values from supplied list, sequentially.
buildTree :: Int -> [b] -> BalTree b
buildTree = invertTree . buildTree'

data BalTree' b = Zero' b
                | Succ' (Pair (BalTree' b))
                
buildTree' :: Int -> [b] -> BalTree' b
buildTree' _       []     = error "Exhausted input!"
buildTree' 0       (x:xs) = Zero' x
buildTree' n       xs     = Succ' ((buildTree' (n-1) (evens xs)), (buildTree' (n-1) (odds xs)))

invertTree :: BalTree' b -> BalTree b
invertTree Zero' x            = Zero x
invertTree Succ' (bt'1, bt'2) = Succ (joinTrees (invertTree bt'1, invertTree bt'2))

-- joinTrees :: Pair (BalTree b) -> BalTree (Pair b)
joinTrees :: Pair (BalTree b) -> BalTree b
joinTrees (Zero x,   Zero y)   = Succ (Zero (x, y))
joinTrees (Succ bt1, Succ bt2) = Succ (joinTrees (bt1, bt2))

