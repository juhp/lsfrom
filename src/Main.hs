module Main (main) where

import Control.Monad.Extra (filterM, unless, when, whenJust)
import Data.List.Extra (dropWhileEnd, unsnoc)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing, maybeToList)
import SimpleCmd (cmdLines, error', (+-+))
import SimpleCmdArgs
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath
import System.Locale.SetLocale (setLocale, Category(LC_COLLATE))

import Collate (beforeUnsafe)
import Paths_lsfrom (version)

data IncludeExclude a = Include a | Exclude a

unIncludeExclue :: IncludeExclude a -> a
unIncludeExclue (Include x) = x
unIncludeExclue (Exclude x) = x

type NEString = NE.NonEmpty Char

showFile :: IncludeExclude NEString -> String
showFile = NE.toList . unIncludeExclue

data Only = OnlyDirs | OnlyFiles

main :: IO ()
main =
  simpleCmdArgs (Just version) "List directories files starting from file"
  "lsfrom lists the files in a directory that follow from the given file" $
  lsfrom
  <$> switchWith 's' "strict" "fail if specified file(s) do not exist"
  <*> switchWith 'A' "all" "include hidden (dot) files"
  <*> optional
  (flagLongWith' OnlyDirs "dirs" "Only list directories" <|>
   flagLongWith' OnlyFiles "files" "Only list files")
  <*> optional
  (Include <$> optNonEmpty 'f' "from" "STARTFILE" "files from STARTFILE" <|>
   Exclude <$> optNonEmpty 'a' "after" "STARTFILE" "files after STARTFILE")
  <*> optional
  (Include <$> optNonEmpty 'u' "until" "LASTFILE" "files until LASTFILE" <|>
   Exclude <$> optNonEmpty 'b' "before" "LASTFILE" "files before LASTFILE")
  where
    optNonEmpty =
      let readNonEmpty = NE.nonEmpty . removeTrailing
          removeTrailing "" = error' "empty filename not allowed"
          removeTrailing f =
            if f == "/"
            then "/"
            else dropWhileEnd (== '/') f
      in optionWith (maybeReader readNonEmpty)

lsfrom :: Bool -> Bool -> Maybe Only -> Maybe (IncludeExclude NEString)
       -> Maybe (IncludeExclude NEString) -> IO ()
lsfrom strict hidden monly mstart mlast = do
  -- required for correct C collation (before below)
  mlocale <- setLocale LC_COLLATE $ Just "" -- use default locale
  when (isNothing mlocale) $ error' "failed to setlocale"
  let dirarg = maybe [] (maybeToList . fst . mdirfile) mstart
      showhidden = hidden || fmap (NE.head . unIncludeExclue) mstart == Just '.'
  listing <- cmdLines "ls" (["-A" | showhidden] ++ dirarg) >>= filterTypes
  when strict $ do
    whenJust mstart $ \start ->
      unless (showFile start `elem` listing) $
      error' $ showFile start +-+ "does not exist"
  let result = takeLast $ dropStart listing -- uses LC_COLLATE
  mapM_ (putStrLn . (renderDir </>)) result
  where
    mdirfile :: IncludeExclude NEString -> (Maybe FilePath, FilePath)
    mdirfile ie =
        case splitFileName $ showFile ie of
          (d,f) ->
            let mdir = if d == "./" then Nothing else Just d
            in (mdir,f)

    filterTypes :: [String] -> IO [String]
    filterTypes =
      case monly of
        Nothing -> return
        Just o ->
          case o of
            OnlyDirs -> filterM doesDirectoryExist
            OnlyFiles -> filterM doesFileExist

    dropStart :: [FilePath] -> [FilePath]
    dropStart files =
      case mstart of
        Nothing -> files
        Just start ->
          case mdirfile start of
            (_,file) ->
              case dropWhile (`beforeUnsafe` file) files of
                [] -> []
                sf@(f:fs) ->
                  case start of
                    Include _ -> sf
                    Exclude e -> if f == NE.toList e then fs else sf

    takeLast files =
      case mlast of
        Nothing -> files
        Just ie ->
          case dropWhileEnd (showFile ie `beforeUnsafe`) files of
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
