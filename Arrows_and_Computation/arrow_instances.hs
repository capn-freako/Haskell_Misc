import Prelude hiding (pure)

class Arrow a where
    pure  :: (b -> c) -> a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (b, d) (c, d)

second :: Arrow a => a b c -> a (d, b) (d, c)
second f = pure swap >>> first f >>> pure swap
    where swap ~(x, y) = (y, x)

assoc :: ((a, b), c) -> (a, (b, c))
assoc ~(~(x, y), z) = (x, (y, z))

unassoc :: (a, (b, c)) -> ((a, b), c)
unassoc ~(x, ~(y, z)) = ((x, y), z)

prod :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(f `prod` g) ~(x, y) = (f x, g y)

instance Arrow (->) where
    pure f  = f
    f >>> g = g . f
    first f = f `prod` id

newtype State s a b = ST {runST :: (s, a) -> (s, b)}

instance Arrow (State s) where
    pure f        = ST (id `prod` f)
    ST f >>> ST g = ST (g . f)
    first (ST f)  = ST (assoc . (f `prod` id) . unassoc)

hole :: ()
-- hole = first ((pure id) :: State String Int Int)
hole = ST (runST $ pure (+ 1))

main :: IO ()
main = do
    print $ (runST (pure (+ 1))) ("state", 1)

