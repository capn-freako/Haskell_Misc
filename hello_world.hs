--------------------------------------------------------------------------------
-- | 
-- Module      : Main
-- Note        : Hello, World!
-- 
-- A dummy Haskell module, for playing.
-- 
--------------------------------------------------------------------------------


-- main :: IO ()
main = putStrLn "Hello, World!"

listProc :: [a] -> [a]
listProc = undefined

f :: Char -> Int
f x = x + 1

f2 :: [Int] -> Int
f2 = foldr (+) 0

