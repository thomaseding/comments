{-# LANGUAGE ViewPatterns #-}

module Comments.C (
  remove
) where

import Control.Monad.ListM (dropWhileM)
import Control.Monad.State.Strict (evalState, put, get)
import Data.List (stripPrefix)

--------------------------------------------------------------------------------

remove :: String -> String
remove = rc

rc :: String -> String
rc ('"':cs) = '"' : string cs
rc (stripPrefix "/*" -> Just str) = let
  st = ('\0', '\0')
  in rc $ (\f -> evalState (dropWhileM f str) st) $ \c -> do
    (prev1, prev2) <- get
    put (prev2, c)
    return $ not $ [prev1, prev2] == "*/"
rc (c:cs) = c : rc cs
rc "" = ""

string :: String -> String
string ('\\':c:cs) = '\\' : c : string cs
string ('"':cs) = '"' : rc cs
string (c:cs) = c : string cs
string "" = ""


