-- Find all incomplete tasks in my personal notes.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date: January 31, 2018
--
-- Copyright (c) 2018 David Banas; all rights reserved World wide.

{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow        ((&&&))
import Control.Exception
import Control.Monad        (forM, forM_)
import Data.Semigroup       ((<>))
import Options.Applicative
import System.Directory     (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath      ((</>))


data Opts = Opts
  { ext        :: String
  , srcd       :: String
  }

opts :: Parser Opts
opts = Opts
      <$> strOption
          ( long "ext"
         <> short 'e'
         <> metavar "EXT"
         <> showDefault
         <> value "page"
         <> help "File extension" )
      <*> argument str
          ( metavar "FILE"
         <> help "Source file/directory in which to search" )


main :: IO ()
main = findTasks =<< execParser opts'
  where
    opts' = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Extract incomplete tasks from personal notes."
     <> header "get_tasks - a task fetcher/consolidator" )


findTasks :: Opts -> IO ()
findTasks Opts{..} = do
  isFile <-doesFileExist      srcd
  isDir  <-doesDirectoryExist srcd
  if isFile then scanFile srcd
            else if isDir then scanDir srcd
                          else error $ "Sorry, but " ++ show srcd ++ " does not exist."


scanFile :: FilePath -> IO ()
scanFile fp = catch (do
  ft <- readFile fp
  let strs = lines ft
      tsks = filter (uncurry (&&) . (and &&& (\xs -> length xs > 4)) . zipWith (==) "- [ ]") strs
  putStr $ unlines tsks)
  (\(SomeException _) -> return ())


scanDir :: FilePath -> IO ()
scanDir fp = do
  fps <- getRecursiveContents fp
  forM_ fps scanFile


-- From Ch. 9 in Real World Haskell
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

