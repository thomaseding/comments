module Comments (
  main
) where

import qualified Comments.C
import qualified Comments.Cpp
import Data.Char (isSpace, toLower)
import Data.List (partition)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let (opts, files) = partition ((== "-") . take 1) args
  if any (`elem` ["-h", "--help"]) opts || null files
    then help
    else mapM_ (removeComments opts) files

help :: IO ()
help = mapM_ putStrLn [
    "Usage: comments [options] files"
  , "Description: Removes comments in files based on filetype."
  , "-l: Remove empty lines and lines with only spaces in them."
  ]

removeComments :: [String] -> FilePath -> IO ()
removeComments opts file = do
  str <- readFile file
  str' <- fmap removeEmptyLines . remove $ str
  length str `seq` writeFile file str'
  where
    removeEmptyLines = if "-l" `elem` opts
      then unlines . filter (not . all isSpace) . lines
      else id
    remove = case map toLower $ drop 1 $ takeExtension file of
      "" -> return
      "c" -> return . Comments.C.remove
      "cpp" -> return . Comments.Cpp.remove
      _ -> \str -> do
        putStrLn $ "Unknown filetype: Skipping " ++ file
        return str


