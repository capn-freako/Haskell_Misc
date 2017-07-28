-- Playing w/ Chachaf's ideas (from Haskell meetup).

{-# LANGUAGE KindSignatures
           , DataKinds
  #-}

data Proxy a = Proxy

-- Simple Peano numbers, with conversion to/from integers.
data Nat = Z
         | S Nat
    deriving (Show, Eq, Ord)

predNat :: Nat -> Nat
predNat     Z = Z
predNat (S n) = n

succNat :: Nat -> Nat
succNat n = S n

decodeNat :: Nat -> Integer
decodeNat Z = 0
decodeNat n = 1 + decodeNat (predNat n)

encodeNat :: Integer -> Nat
encodeNat 0 = Z
encodeNat n = S $ encodeNat (n - 1)

-- Value level arithmetic, via Num instance.
instance Num Nat where
    fromInteger = encodeNat
    n + m       = encodeNat $ decodeNat n + decodeNat m

-- Type level arithmetic and value reflection.
-- KindSignatures and DataKinds language pragmas were added, to support this.
-- newtype NatT i Nat = N i         -- Doesn't work; compiler complains about "Nat".
-- newtype NatT i (n :: Nat) = N i  -- Works, but 'i' seems superfluous.
newtype NatT (n :: Nat) = N { unN :: Nat }

-- Reflect the implied value of a type of kind, Nat.
-- GADTs language pragma added, to support this.
-- data NatT :: Nat -> Nat where


-- data Zero
-- data Succ a

main :: IO ()
main = do
    print $ decodeNat $ S (S (S Z))
    print $ encodeNat $ 3
    let x = encodeNat 3
    let y = encodeNat 4
    print $ decodeNat $ x + y

