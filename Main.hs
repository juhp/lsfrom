module Main (main) where

import SimpleCmd
import SimpleCmdArgs
import System.FilePath

import Paths_lsfrom (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "List files from pattern"
  "lsfrom lists the files in a directory that follow from the given pattern" $
  lsfrom <$> switchWith 'a' "all" "include hidden (dot) files" <*> strArg "FILEPAT"

lsfrom :: Bool -> FilePath -> IO ()
lsfrom hidden file = do
  let isdir = last file == pathSeparator
      file' = if isdir then takeDirectory file else file
      (dir,pat) = splitFileName file'
      pat' = pat ++ if isdir then [pathSeparator] else ""
  sorted <- do
    files <- cmd "ls" $ ["-a" | hidden || head pat == '.'] ++ [dir]
    lines <$> cmdStdIn "sort" [] (pat' ++ "\n" ++ files)
  let result = tail $ dropWhile (pat' /=) sorted
  mapM_ (putStrLn . (renderDir dir </>)) result
  where
    renderDir dir = if dir == "./" then "" else dir
