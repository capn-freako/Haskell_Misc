module Main where

import Data.Char (ord, chr)

-- Our "bit" representation.
data Bit = O  -- 0
         | I  -- 1
    deriving (Show, Eq)
    
-- Generic conversions from Int to [Bit] and back.
intToBits :: Int -> [Bit]
intToBits n | n < 0          = error "No negative Ints!"
            | n == 0         = []
            | n `mod` 2 /= 0 = I : msbs
            | otherwise      = O : msbs
    where msbs = intToBits (n `div` 2)
            
bitsToInt :: [Bit] -> Int
bitsToInt = bitsToInt' 1

bitsToInt' :: Int -> [Bit] -> Int
bitsToInt' _ []                 = 0
bitsToInt' w (b:bs) | b == I    = w + bitsToInt' w' bs
                    | otherwise = 0 + bitsToInt' w' bs
    where w' = w * 2

-- Typeclass representing compressable data types.
class Compressable t where
    -- bitLen  :: Int  -- Why doesn't this work?
    bitLen  :: t -> Int
    toInt   :: t -> Int
    fromInt :: Int -> t
    
    -- the "generic" functions
    compress :: t -> [Bit]
    compress x | neededFill < 0 = error "Value overflow!"
               | otherwise      = prelimRes ++ (take neededFill (repeat O))
        where prelimRes  = intToBits $ toInt x
              neededFill = bitLen x - length prelimRes

    uncompress :: [Bit] -> (t, [Bit])  -- Returns the unconsumed bits.
    uncompress bs | length bs < bitLen x = error "Insufficient bits!"
                  | otherwise            = ( x, drop (bitLen x) bs)
        where x = fromInt (bitsToInt (take (bitLen x) bs))

instance Compressable Int where
    bitLen _ = 32
    toInt    = id
    fromInt  = id
    
instance Compressable Char where
    bitLen _ = 7
    toInt    = ord
    fromInt  = chr

instance Compressable a => Compressable [a] where
    bitLen  xs = sum $ map bitLen xs
    toInt      = undefined
    fromInt    = undefined
    
    compress []     = O : []
    compress (x:xs) = I : compress x ++ compress xs

    uncompress []              = error "Empty list!"
    uncompress (b:bs) | b == O = ([]           , bs)
                      | b == I = (head' : tail', unusedBits)
        where head'      = fst recRes
              tail'      = fst $ recRes'
              unusedBits = snd $ recRes'
              recRes     = uncompress bs
              recRes'    = uncompress $ snd recRes

instance (Compressable a, Compressable b) => Compressable (a, b) where
    bitLen (x, y) = bitLen x + bitLen y
    toInt         = undefined
    fromInt       = undefined
    
    compress (x, y) = compress x ++ compress y
    
    uncompress bs = let (x, bs')  = uncompress bs
                        (y, bs'') = uncompress bs'
                    in ((x, y), bs'')

main :: IO ()
main = do
    let x = compress (5 :: Int)
    print x
    let y = compress ('A' :: Char)
    print y
    let z = compress ['A', 'B']
    print z
    print $ compress ('A', 5 :: Int)
    print $ compress 'A' == compress 'A'
    print $ compress 'A' == compress 'B'
    print $ ((uncompress x) :: (Int, [Bit]))
    print $ ((uncompress y) :: (Char, [Bit]))
    print $ ((uncompress z) :: ([Char], [Bit]))

