module Main (main) where

import Control.Monad.Extra (unless, when, whenJust)
import Data.List.Extra (dropWhileEnd, unsnoc)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import SimpleCmd (cmdLines, error', (+-+))
import SimpleCmdArgs
import System.FilePath

import Paths_lsfrom (version)

data IncludeExclude a = Include a | Exclude a

unIncludeExclue :: IncludeExclude a -> a
unIncludeExclue (Include x) = x
unIncludeExclue (Exclude x) = x

type NEString = NE.NonEmpty Char

showFile :: IncludeExclude NEString -> String
showFile = NE.toList . unIncludeExclue

main :: IO ()
main =
  simpleCmdArgs (Just version) "List directories files starting from file"
  "lsfrom lists the files in a directory that follow from the given file" $
  lsfrom
  <$> switchWith 's' "strict" "fail if specified file(s) do not exist"
  <*> switchWith 'A' "all" "include hidden (dot) files"
  <*> optional
  (Include <$> optionWith (maybeReader readNonEmpty) 'f' "from" "STARTFILE" "files from STARTFILE" <|>
   Exclude <$> optionWith (maybeReader readNonEmpty) 'a' "after" "STARTFILE" "files after STARTFILE")
  <*> optional
  (Include <$> optionWith (maybeReader readNonEmpty) 'u' "until" "LASTFILE" "files until LASTFILE" <|>
   Exclude <$> optionWith (maybeReader readNonEmpty) 'b' "before" "LASTFILE" "files before LASTFILE")
  where
    removeTrailing f =
      if f == "/"
      then "/"
      else dropWhileEnd (== '/') f

    readNonEmpty = NE.nonEmpty . removeTrailing

lsfrom :: Bool -> Bool -> Maybe (IncludeExclude NEString)
       -> Maybe (IncludeExclude NEString) -> IO ()
lsfrom strict hidden mstart mlast = do
  let dirarg = maybe [] (maybeToList . fst . mdirfile) mstart
      showhidden = hidden || fmap (NE.head . unIncludeExclue) mstart == Just '.'
  listing <- cmdLines "ls" $ ["-A" | showhidden] ++ dirarg
  when strict $ do
    whenJust mstart $ \start ->
      unless (showFile start `elem` listing) $
        error' $ showFile start +-+ "does not exist"
  let result = takeLast $ dropStart listing
  mapM_ (putStrLn . (renderDir </>)) result
  where
    mdirfile :: IncludeExclude NEString -> (Maybe FilePath, FilePath)
    mdirfile ie =
        case splitFileName $ showFile ie of
          (d,f) ->
            let mdir = if d == "./" then Nothing else Just d
            in (mdir,f)

    dropStart :: [FilePath] -> [FilePath]
    dropStart files =
      case mstart of
        Nothing -> files
        Just start ->
          case mdirfile start of
            (_,file) ->
              case dropWhile (< file) files of
                [] -> []
                sf@(f:fs) ->
                  case start of
                    Include _ -> sf
                    Exclude e -> if f == NE.toList e then fs else sf

    takeLast files =
      case mlast of
        Nothing -> files
        Just ie ->
          case dropWhileEnd (> showFile ie) files of
            [] -> []
            lf ->
              case ie of
                Include _ -> lf
                Exclude e ->
                  case unsnoc lf of
                    Nothing -> []
                    Just (ls,l) ->
                      if l == NE.toList e then ls else lf

    renderDir =
      case fst . mdirfile <$> mstart of
        Just (Just dir) -> dir
        _ -> ""
