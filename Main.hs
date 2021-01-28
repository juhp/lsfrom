module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [start] -> do
      files <- sort <$> (getCurrentDirectory >>= listDirectory)
      mapM_ (printFrom start) files
    _ -> error "Usage: lsfrom [FILE]"

printFrom :: String -> String -> IO ()
printFrom start f =
  when (f >= start) $ putStrLn f
