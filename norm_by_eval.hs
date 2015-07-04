-- Ch. 5 of "Fun with Phantom Types" by Hinze
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module NormForm where

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
reflect RBase e = In e
reflect (ra :-> rb) e = reflect rb . App e . reify ra

-- Exercise 12 - Implement show() for 'Term t'.
instance Show (Term t) where
    show (Var s) = s
    show (App f x) = "App (" ++ show f ++ "(" ++ show x ++ "))"
    show (Fun rf) = "Fun (\\a -> " ++ show (rf (Var "a")) ++ ")"

