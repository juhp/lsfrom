module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then error "Usage: lsfrom [FILE]"
    else do
    let start = head args
    files <- sort <$> (getCurrentDirectory >>= listDirectory)
    mapM_ (printFrom start) files

printFrom :: String -> String -> IO ()
printFrom start f =
  when (f >= start) $ putStrLn f

