module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  args <- getArgs
  case args of
    [start] -> do
      let (dir,pat) = splitFileName start
      files <- sort <$> listDirectory dir
      mapM_ (printFrom (render dir) pat) files
    _ -> error "Usage: lsfrom [FILE]"
  where
    render dir = if dir == "./" then "" else dir

printFrom :: FilePath -> String -> String -> IO ()
printFrom dir start f =
  when (f >= start) $ putStrLn $ dir </> f
