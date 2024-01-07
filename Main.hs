module Main (main) where

import Data.List (dropWhileEnd)
import SimpleCmd
import SimpleCmdArgs
import Safe (tailSafe)
import System.FilePath

import Paths_lsfrom (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "List files from pattern"
  "lsfrom lists the files in a directory that follow from the given pattern" $
  lsfrom
  <$> switchWith 's' "strict" "fail if specified file(s) do not exist"
  <*> switchWith 'A' "all" "include hidden (dot) files"
  <*> switchWith 'a' "after" "files after STARTFILE [default: from STARTFILE]"
  <*> optional (removeTrailing <$> strOptionWith 'u' "until" "LASTFILE" "files until FILE")
  <*> switchWith 'b' "before" "files before LASTFILE [default: until LASTFILE]"
  <*> (removeTrailing <$> strArg "STARTFILE")
  where
    removeTrailing "" = ""
    removeTrailing "/" = "/"
    removeTrailing f =
      if last f == '/'
      then removeTrailing $ init f
      else f

lsfrom :: Bool -> Bool -> Bool -> Maybe FilePath -> Bool -> FilePath -> IO ()
lsfrom strict hidden after muntil before file =
  case reverse (splitDirectories file) of
    [] -> error' "empty filename!"
    ("":_) -> error' "empty filename!"
    (entry@(e:_):revdir) -> do
      let dir = joinPath $ reverse revdir
      lsout <- cmd "ls" $
                 ["-A" | hidden || e == '.'] ++ [dir | not (null dir)]
      let lsEntries = lines lsout
      whenJust muntil $ \until' -> do
        when (strict && until' `notElem` lsEntries) $
          error' $ until' +-+ "not found in directory" +-+ dir
      let entryExists = entry `elem` lsEntries
          muntilExists =
            case muntil of
              Nothing -> Nothing
              Just until' ->
                Just (until', until' `elem` lsEntries)
      listingWith <-
        lines <$>
        if strict
        then do
          unless entryExists $
            error' $ entry +-+ "not found in directory" +-+ dir
          return lsout
        else if entryExists
             then case muntilExists of
                    Just (until',False) ->
                      sortLs $ prepend until' lsout
                    _ -> return lsout
             else case muntilExists of
                    Just (until',False) ->
                      sortLs $
                      if entry == until'
                      then prepend entry lsout
                      else prepend entry $ prepend until' lsout
                    _ -> sortLs $ prepend entry lsout
      let result =
            takeUntil muntilExists $
            (if after || not entryExists then tailSafe else id) $
            dropWhile (entry /=) listingWith
      mapM_ (putStrLn . (renderDir dir </>)) result
  where
    renderDir dir = if dir == "./" then "" else dir

    sortLs :: String -> IO String
    sortLs = cmdStdIn "sort" []

    prepend :: String -> String -> String
    prepend miss ls = miss ++ '\n' : ls

    takeUntil :: Maybe (String,Bool) -> [String] -> [String]
    takeUntil Nothing es = es
    takeUntil (Just _) [] = []
    takeUntil (Just (until',exists)) es =
      case dropWhileEnd (until' /=) es of
        [] -> []
        es' ->
          (if before || not exists then init else id) es'
