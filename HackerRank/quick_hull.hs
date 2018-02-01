-- Haskell implementation of "Quick Hull" algorithm.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   August 28, 2016
--
-- Copyright (c) 2016 David Banas; all rights reserved World wide.

import Control.Monad (replicateM)

type Point = (Int, Int)

quickHull :: [Point] -> [Point]
quickHull ps | length ps < 3 = error "I need, at least, 3 points."
             | otherwise     = quickHull' ps p1 p2 ++ quickHull' ps p2 p1
  where p1  = head ps'
        p2  = last ps'
        ps' = quickSort fst ps

quickHull' :: [Point] -> Point -> Point -> [Point]
quickHull' [] p1 p2                                = [p1]
quickHull' ps p1 p2 | not $ any (rightOf p2 p1) ps = [p1]
                    | otherwise                    = quickHull' ps' p1 p3 ++ quickHull' ps' p3 p2
                                                      where ps' = removeFromTriangle p1 p2 p3 ps
                                                            p3  = furthestAboveLine p1 p2 ps

quickSort :: Ord b => (a -> b) -> [a] -> [a]
quickSort _ []     = []
quickSort f (x:xs) = quickSort f (filter ((<= y) . f) xs) ++ [x] ++ quickSort f (filter ((> y) . f) xs) 
  where y = f x

-- Assumes vertices listed in counterclockwise order.
removeFromTriangle :: Point -> Point -> Point -> [Point] -> [Point]
removeFromTriangle p1 p2 p3 ps = filter (\p -> rightOf p1 p2 p || rightOf p2 p3 p || rightOf p3 p1 p) ps'
  where ps' = filter (\x -> x /= p1 && x /= p2 && x /= p3) ps

rightOf :: Point -> Point -> Point -> Bool
rightOf p1 p2 p = crossProd (fst p2 - fst p1, snd p2 - snd p1) (fst p - fst p1, snd p - snd p1) < 0

crossProd :: Point -> Point -> Int
crossProd p1 p2 = fst p1 * snd p2 - snd p1 * fst p2

-- 'relDistFromLine' does not give the correct absolute distance from a point to a line!
-- (It does give the correct relative distance.)
furthestAboveLine :: Point -> Point -> [Point] -> Point
furthestAboveLine p1 p2 = last . quickSort (relDistFromLine p1 p2) . filter (rightOf p2 p1)
  where relDistFromLine :: Point -> Point -> Point -> Int
        relDistFromLine q1 q2 p = abs $ crossProd p dp - crossProd q1 q2
           where dp = (fst q2 - fst q1, snd q2 - snd q1)

points2perim :: [Point] -> Double
points2perim ps | length ps < 3 = 0.0
                | otherwise     = sum $ zipWith points2dist ps (tail ps ++ [head ps])

points2dist :: Point -> Point -> Double
points2dist p1 p2 = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2
  where x1 = fromIntegral $ fst p1
        y1 = fromIntegral $ snd p1
        x2 = fromIntegral $ fst p2
        y2 = fromIntegral $ snd p2

list2pair :: [a] -> (a, a)
list2pair []       = undefined
list2pair (x : xs) = (x, head xs)

main :: IO ()
main = do
    num_points <- readLn
    points <- Control.Monad.replicateM num_points ((list2pair . map read . words) `fmap` getLine)
--    print (points :: [Point])
--    print $ qsort' snd points
--    let points' = [ps | ps <- subsets points, length ps == 3]
    let points' = quickHull points
    print $ points2perim points'
--    putStrLn "Points\t\t\tPerimeter\tCurl"
--    sequence_ $ for points' $ \p -> do
--        printf "%22s       %5.1f      %.3f\n" (show p) (points2perim p) (curl p)

