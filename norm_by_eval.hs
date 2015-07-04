-- Ch. 5 of "Fun with Phantom Types" by Hinze
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

newtype Base = In{out :: Term Base}

infixr :->
data Type t where
    RBase :: Type Base
    (:->) :: Type a -> Type b -> Type (a -> b)

b :: Type Base
b = RBase

data Term t where
    App :: Term (a -> b) -> Term a -> Term b
    Fun :: (Term a -> Term b) -> Term (a -> b)
    Var :: String -> Term t

reify :: forall t. Type t -> t -> Term t
reify RBase v = out v
reify (ra :-> rb) v = Fun (reify rb . v . reflect ra)

reflect :: forall t. Type t -> Term t -> t
reflect RBase expr = In expr
reflect (ra :-> rb) expr = reflect rb . App expr . reify ra

-- Exercise 12 - Implement show() for 'Term t'.
instance Show (Term t) where
    show (Var str) = str
    show (App f x) = "App " ++ show f ++ " (" ++ show x ++ ")"
    show (Fun rf) = "Fun (\\a -> " ++ show (rf (Var "a")) ++ ")"

s x y z = x z (y z)
k x y = x
i x = x
e =s (s (k s) (s (k k) i))((s ((s (k s))((s (k k))i)))(k i))
main = do print $ reify (b :-> b) (s k k)
          print $ reify (b :-> (b :-> b)) (s (k k)i)
          print $ reify ((b :-> b) :-> (b :-> b)) e

