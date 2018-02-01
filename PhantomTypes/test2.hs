module Main where

main :: IO ()
main = do
    -- let x = compress (5 :: Int)
    let x = 5 :: Int
    print x
    {-
    let y = compress ('A' :: Char)
    print y
    let z = compress ['A', 'B']
    print z
    print $ compress ('A', 5 :: Int)
    print $ compress 'A' == compress 'A'
    print $ compress 'A' == compress 'B'
    print $ ((uncompress x) :: (Int, [Bit]))
    print $ ((uncompress y) :: (Char, [Bit]))
    print $ ((uncompress z) :: ([Char], [Bit]))
    -}

