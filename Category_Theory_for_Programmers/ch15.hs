-- Answers to challenges in Ch. 15 of "Category Theory for Programmers" by Bartosz Milewski
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   November 29, 2018
--
-- Copyright (c) 2018 David Banas; all rights reserved World wide.

-- Ex. 1 - Show that the two functions phi and psi that form the Yoneda isomorphism in Haskell are inverses of each other.
phi :: (forall x . (a -> x) -> F x) -> F a
phi alpha = alpha id

psi :: F a -> (forall x . (a -> x) -> F x)
psi fa h = fmap h fa

-- phi . psi                    =  {definition of composition}
-- \fa -> phi (psi fa)          =  {definition of `psi`}
-- \fa -> phi (\h -> fmap h fa) =  {definition of `phi`}
-- \fa -> fmap id fa            =  {Functor law}
-- \fa -> fa                    =  {definition of `id`}
-- id

-- psi . phi                 =  {definition of composition}
-- \alpha -> psi (phi alpha) =  {definition of `phi`}
-- \alpha -> psi (alpha id)  =  {definition of `psi`}
-- \alpha -> fmap id alpha   =  {Functor law}
-- \alpha -> alpha           =  {definition of `id`}
-- id

-- ==> Q.E.D.

-- Ex. 2 - A discrete category is one that has objects but no morphisms other than identity morphisms. How does the Yoneda lemma work for functors from such a category?
--
-- Let c1 and c2 be objects in a discrete category, C.
-- Let F be a functor from C to D.
-- Then the corresponding objects in D are: (F c1) and (F c2).
