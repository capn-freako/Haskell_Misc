-- Ch. 5 of "Fun with Phantom Types" by Hinze
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.State

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
allNames :: [String]
allNames = map reverse $ tail allNames' where
    allNames' = "" : [suf : base | base <- allNames', suf <- ['a'..'z']]

instance Show (Term t) where
    show x = evalState (showTerm x) allNames

showTerm :: Term t -> State [String] String
showTerm (Var str) = return str
showTerm (App f (Var str)) = do
    fStr <- showTerm f
    return $ "App " ++ fStr ++ " " ++ str
showTerm (App f x) = do
    fStr <- showTerm f
    xStr <- showTerm x
    return $ "App " ++ fStr ++ " (" ++ xStr ++ ")"
showTerm (Fun rf) = do
    varNames <- get
    let varName = head varNames
    put $ tail varNames
    rfStr <- showTerm (rf (Var varName))
    return $ "Fun (\x3BB" ++ varName ++ " -> " ++ rfStr ++ ")"

-- Hinze's interactive testing repeated, here.
s :: forall t t1 t2. (t2 -> t1 -> t) -> (t2 -> t1) -> t2 -> t
s x y z = x z (y z)

k :: forall t t1. t1 -> t -> t1
k x _ = x

i :: forall t. t -> t
i x = x

e =s (s (k s) (s (k k) i))((s ((s (k s))((s (k k))i)))(k i))

main :: IO ()
main = do print $ reify (b :-> b) (s k k)
          print $ reify (b :-> (b :-> b)) (s (k k)i)
          print $ reify ((b :-> b) :-> (b :-> b)) e

