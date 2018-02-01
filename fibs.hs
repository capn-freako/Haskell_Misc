fibs :: [Integer]
fibs = [0, 1] ++ zipWith (+) fibs (drop 1 fibs)

main = do
    let ans = take 5 fibs
    print $ show ans

