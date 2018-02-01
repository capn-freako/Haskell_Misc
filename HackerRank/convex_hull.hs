-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad (replicateM)
import Text.Printf

-- type Point = (Double, Double)
type Point = (Int, Int)

convexHull :: [Point] -> [Point]
convexHull ps = fst $ head $ sortSnd [(ps', points2perim ps') | ps' <- subsets ps, length ps' > 2, all (ps' `contains`) ps]

qsort' :: Ord b => (a -> b) -> [a] -> [a]
qsort' _ []     = []
qsort' f (x:xs) = [y | y <- qsort' f xs, f y <= f x] ++ [x] ++ [y | y <- qsort' f xs, f y > f x] 

sortSnd :: Ord b => [(a, b)] -> [(a, b)]
sortSnd = qsort' snd

points2perim :: [Point] -> Double
points2perim ps | length ps < 3 = 0.0
                | otherwise     = sum $ zipWith points2dist ps (tail ps ++ [head ps])

points2dist :: Point -> Point -> Double
points2dist p1 p2 = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2
  where x1 = fromIntegral $ fst p1
        y1 = fromIntegral $ snd p1
        x2 = fromIntegral $ fst p2
        y2 = fromIntegral $ snd p2

subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- If a continuous traversal of the perimeter of a convex polygon
-- yields the list of points, ps, then a point, x, is contained by that
-- polygon, if when translated so as to make x the new origin, the
-- cross products of all pairs of vectors formed by adjacent elements
-- of ps (including a pairing of the last with the first, in that order)
-- have the same sign.
contains :: [Point] -> Point -> Bool
ps `contains` x | length ps < 3 = False
                | colinear ps   = False
                | x `elem` ps   = True
                | otherwise     = same $ map signum $ zipWith crossProd ps' (tail ps' ++ [head ps'])
                                      where ps' = [(fst p - dx, snd p - dy) | p <- ps]
                                            dx  = fst x
                                            dy  = snd x

colinear :: [Point] -> Bool
colinear ps | length ps < 3 = True
            | otherwise     = all (pointOnLine m b) $ drop 2 ps
                                  where m  = dy / dx
                                        b  = y1 - m * x1
                                        dx = x2 - x1
                                        dy = y2 - y1
                                        x1 = fromIntegral $ fst p1
                                        y1 = fromIntegral $ snd p1
                                        x2 = fromIntegral $ fst p2
                                        y2 = fromIntegral $ snd p2
                                        p1 = head ps
                                        p2 = head $ tail ps

same :: Eq a => [a] -> Bool
same [] = True
same xs = all (== head xs) $ tail xs

crossProd :: Point -> Point -> Int
crossProd p1 p2 = fst p1 * snd p2 - snd p1 * fst p2

pointOnLine :: Double -> Double -> Point -> Bool
pointOnLine m b p = y == m * x + b
  where x = fromIntegral $ fst p
        y = fromIntegral $ snd p

list2pair :: [a] -> (a, a)
list2pair []       = undefined
list2pair (x : xs) = (x, head xs)

for = flip map

main :: IO ()
main = do
    num_points <- readLn
    points <- Control.Monad.replicateM num_points ((list2pair . map read . words) `fmap` getLine)
--    print (points :: [Point])
--    print $ qsort' snd points
--    let points' = [ps | ps <- subsets points, length ps == 3]
    let points' = convexHull points
    print $ points2perim points'
--    putStrLn "Points\t\t\tPerimeter\tCurl"
--    sequence_ $ for points' $ \p -> do
--        printf "%22s       %5.1f      %.3f\n" (show p) (points2perim p) (curl p)

