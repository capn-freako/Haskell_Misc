-- Enter your code here. Read input from STDIN. Print output to STDOUT
list2pair :: [a] -> (a, a)
list2pair []       = undefined
list2pair (x : xs) = (x, head xs)

type Point = (Double, Double)

points2area :: [Point] -> Double
points2area ps | length ps < 3 = 0.0
               | otherwise     = abs $ sum (zipWith (*) (zipWith (+) xs xs') (zipWith (-) ys ys')) / 2.0
                                  where xs  = fst $ unzip ps
                                        ys  = snd $ unzip ps
                                        xs' = tail xs ++ [head xs]
                                        ys' = tail ys ++ [head ys]

main :: IO ()
main = do
    num_points <- readLn
    points <- sequence $ replicate num_points $ (list2pair . map read . words) `fmap` getLine
    print $ points2area points

