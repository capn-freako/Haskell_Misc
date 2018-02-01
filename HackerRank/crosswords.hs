-- Haskell implementation of "Crossword Solver" algorithm.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   August 29, 2016
--
-- Copyright (c) 2016 David Banas; all rights reserved World wide.

import Control.Arrow
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Text as T

crossSolve :: [String] -> String -> [String]
crossSolve rows wordStr =
    case wordFits rows wordStr of
        Left err -> error err
        Right xs -> if null solutions
                        then error "ERROR: No solutions found."
                        else uncurry (zipWith combStr) $ head solutions
            where solutions = filter crossCheck xs

combStr :: String -> String -> String
combStr = zipWith combChar

combChar :: Char -> Char -> Char
combChar '-' x  = x
combChar  x  _  = x

wordFits :: [String] -> String -> Either String [([String], [String])]
wordFits rows wordStr | numWords /= numSlots =
                            Left $ "ERROR: Number of words: " ++ show numWords ++ ", must match number of slots: " ++ show numSlots
                      | null candidates =
                            Left "ERROR: No word fits found!"
                      | otherwise =
                            Right candidates
  where numWords    = length xwords
        numSlots    = length rowWords + length colWords 
        rowGroups   = map group rows
        colGroups   = map group $ transpose rows
        rowWords    = concat $ filter (not . null) $ map (filter (\l -> startsWith '-' l && longerThan 1 l)) rowGroups
        colWords    = concat $ filter (not . null) $ map (filter (\l -> startsWith '-' l && longerThan 1 l)) colGroups
        xwords      = map T.unpack $ T.split (== ';') $ T.pack wordStr
        wordLengths = map length rowWords ++ map length colWords
        trials      = [(take (length rowWords) trial, drop (length rowWords) trial) |
                            trial <- filter (and . zipWith (==) wordLengths . map length) $ permutations xwords]
        candidates  = map (second transpose . subWords rowGroups colGroups) trials

subWords :: [[String]] -> [[String]] -> ([String],[String]) -> ([String],[String])
subWords rowGroups colGroups wordFit = (rowAssemblage, colAssemblage)
  where rowAssemblage = map concat $ evalState (foldM doSubs [] rowGroups) (fst wordFit)
        colAssemblage = map concat $ evalState (foldM doSubs [] colGroups) (snd wordFit)

doSubs :: [[String]] -> [String] -> State [String] [[String]]
doSubs results blocks = do
    xwords <- get
    let (result, remWords) = runState (foldM doSub [] blocks) xwords
    put remWords
    return $ results ++ [result]

doSub :: [String] -> String -> State [String] [String]
doSub results block =
    if isPlaceHolder block
     then do (w:ws) <- get
             put ws
             return (results ++ [w])
     else return (results ++ [block])

isPlaceHolder :: String -> Bool
isPlaceHolder s = startsWith '-' s && longerThan 1 s

crossCheck :: ([String], [String]) -> Bool
crossCheck = all (uncurry rowCheck) . uncurry zip

rowCheck :: String -> String -> Bool
rowCheck s1 s2 = and (zipWith cellCheck s1 s2)

cellCheck :: Char -> Char -> Bool
cellCheck '-' _  = True
cellCheck  _ '-' = True
cellCheck  x  y  = x == y

startsWith :: Char -> String -> Bool
startsWith _ [] = False
startsWith c cs = head cs == c

longerThan :: Int -> String -> Bool
longerThan n s = length s > n

main :: IO ()
main = do
    rows    <- replicateM 10 getLine
    wordStr <- getLine
    sequence_ $ map putStrLn $ crossSolve rows wordStr

