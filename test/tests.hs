{-# LANGUAGE CPP #-}

import Control.Monad
import SimpleCmd
import System.Directory

lsfrom :: (FilePath, String, [String]) -> IO ()
lsfrom (dir, arg, expect) = do
  out <- withCurrentDirectory dir $
         cmdLines "lsfrom" [arg]
  unless (out == expect) $ do
    cmdN "lsfrom" [arg]
    putStrLn $ "returned> " ++ show out
    putStrLn $ "expected> " ++ show expect
    error' "failed"

tests :: [(FilePath, String,[String])]
tests =
  [ ("test/files", "B", ["B","C"])
  , ("test", "files/B/", ["files/C"])
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
