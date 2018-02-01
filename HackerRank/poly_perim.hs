-- Enter your code here. Read input from STDIN. Print output to STDOUT
list2pair :: [a] -> (a, a)
list2pair []       = undefined
list2pair (x : xs) = (x, head xs)

data Point = Point {
                x_coord :: Double
              , y_coord :: Double
             }

points2perim :: [Point] -> Double
points2perim ps | length ps < 3 = 0.0
                | otherwise     = sum $ zipWith points2dist ps (tail ps ++ [head ps])

points2dist :: Point -> Point -> Double
points2dist p1 p2 = sqrt $ (x_coord p1 - x_coord p2) ** 2 + (y_coord p1 - y_coord p2) ** 2


main :: IO ()
main = do
    num_points <- readLn
    points <- sequence $ replicate num_points $ (uncurry Point . list2pair . map read . words) `fmap` getLine
    print $ points2perim points

