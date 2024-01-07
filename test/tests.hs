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
  [ ("test/files", ["B"], ["B","C"])
  , ("test/files", ["-a", "B"], ["C"])
  , ("test", ["files/C/"], ["files/C"])
  , ("test/files", ["Bb"], ["C"])
  , ("test/files", ["-u", "B", "A"], ["A", "B"])
  , ("test/files", ["-u", "C", "-b", "A"], ["A", "B"])
  , ("test/files", ["-u", "C", "-b", "-a", "A"], ["B"])
  , ("test/files", ["-u", "A", "-b", "-a", "C"], [])
  , ("test/files", ["-u", "B", "-b", "B"], [])
  , ("test/files", ["-u", "B", "-a", "B"], [])
  , ("test/files", ["-u", "B", "-a", "-b", "B"], [])
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
