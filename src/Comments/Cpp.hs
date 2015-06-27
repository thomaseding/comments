{-# LANGUAGE ViewPatterns #-}

module Comments.Cpp (
  remove
) where

import qualified Comments.C
import Data.List (stripPrefix)

--------------------------------------------------------------------------------

remove :: String -> String
remove = rc . Comments.C.remove

rc :: String -> String
rc ('"':cs) = '"' : string cs
rc (stripPrefix "//" -> Just str) = rc $ dropWhile (`notElem` "\r\n") str
rc (c:cs) = c : rc cs
rc "" = ""

string :: String -> String
string ('\\':c:cs) = '\\' : c : string cs
string ('"':cs) = '"' : rc cs
string (c:cs) = c : string cs
string "" = ""

