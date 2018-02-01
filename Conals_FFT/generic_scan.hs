{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Traversable
import Data.Foldable

data Tree a = Node a [Tree a]
    deriving (Functor, Traversable, Foldable, Show)
    
main :: IO ()
main = print $ mapAccumL (\acc e -> (acc + e, acc + e)) 0 demo

demo :: Tree Int
demo =
   Node 1 [Node 20
            [Node 300 [Node 400 []
                      , Node 500 []]
            , Node 310 []
            ]
          , Node 30 [], Node 40 []
          ]

