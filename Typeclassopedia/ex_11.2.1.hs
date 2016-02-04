-- Typeclassopedia Exercise 11.2.1
--
-- Original author: David Banas
-- Original date:   Feb 1, 2016

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Show, Ord, Eq)

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap g (Leaf x)       = Leaf (g x)
  fmap g (Node t1 x t2) = Node (fmap g t1) (g x) (fmap g t2)

instance Foldable Tree where
   foldMap _ Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

t   :: Tree [Int]
t   = (Node (Leaf [1]) [4] (Leaf [3]))

res :: [Tree Int]
res = foldMap (fmap Leaf) t

main :: IO ()
main = do
  print t
  print res

