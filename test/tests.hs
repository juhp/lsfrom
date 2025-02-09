{-# LANGUAGE CPP #-}

import Control.Monad
import SimpleCmd
import System.Directory

type Test = (FilePath, [String], [String])

lsfrom :: Test -> IO ()
lsfrom (dir, args, expect) = do
  out <- withCurrentDirectory dir $
         cmdLines "lsfrom" args
  unless (out == expect) $ do
    cmdN "lsfrom" args
    putStrLn $ "returned> " ++ show out
    putStrLn $ "expected> " ++ show expect
    error' "failed"

tests :: [Test]
tests =
  [ ("test/files", ["-f", "B"], ["B","C"])
  , ("test/files", ["-a", "B"], ["C"])
  , ("test", ["-f", "files/C/"], ["files/C"])
  , ("test/files", ["-f", "Bb"], ["C"])
  , ("test/files", ["-u", "B", "-f", "A"], ["A", "B"])
  , ("test/files", ["-b", "C", "-f", "A"], ["A", "B"])
  , ("test/files", ["-b", "C", "-a", "A"], ["B"])
  , ("test/files", ["-b", "A", "-a", "C"], [])
  , ("test/files", ["-b", "B", "-f", "B"], [])
  , ("test/files", ["-u", "B", "-a", "B"], [])
  , ("test/files", ["-b", "B", "-a", "B"], [])
  , ("test/files", ["-s", "-u", "B", "-f", "A"], ["A", "B"])
  ]

main :: IO ()
main = do
  mapM_ lsfrom tests
  putStrLn $ show (length tests) ++ " tests run"

#if !MIN_VERSION_simple_cmd(0,1,4)
error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
