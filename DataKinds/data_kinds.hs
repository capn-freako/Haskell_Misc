-- Exploring data kinds, after BAHUG talk by Eric Easley Nov. 12, 2015

{-# LANGUAGE DataKinds #-}

module Main where

data SafeList = NonEmpty  -- Creates a type, 'SafeList', and a value constructor, 'NonEmpty'.
                          -- Due to 'DataKinds' extension, also creates a kind, 'SafeList',
                          -- and a type constructor, 'NonEmpty' (or, ''NonEmpty').

-- makeSafe : ('[] ~> 'NonEmpty)
-- makeSafe [] = [0]
-- makeSafe _  = id

head' :: ([a] : 'NonEmpty) -> a
head' = head

main :: IO ()
main = print $ head' ([] :: [Int])

