-- Enter your code here. Read input from STDIN. Print output to STDOUT
is_function :: [(Int, Int)] -> Bool
is_function pairs = no_dups (map fst pairs)

no_dups :: Eq a => [a] -> Bool
no_dups []     = True
no_dups (x:xs) = (not (x `elem` xs)) && (no_dups xs)

swap f = \y -> \x -> f x y

for = swap map

list2pair :: [a] -> (a, a)
list2pair []       = undefined
list2pair (x : xs) = (x, head xs)

main :: IO [()]
main = do
    num_tests <- readLn
    sequence $ take num_tests $ repeat $ do
        num_pairs <- readLn
        pairs <- sequence $ take num_pairs $ repeat $ (list2pair . map read . words) `fmap` getLine
        if is_function pairs
            then (putStrLn "YES")
            else (putStrLn "NO")

