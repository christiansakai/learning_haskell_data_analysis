#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --package regex-posix
-}
module RegexFiltering where

import Text.Regex.Posix ((=~))
import System.Environment (getArgs)

main :: IO ()
main = do
  (myRegex:filenames) <- getArgs
  mapM_ (\filename -> myGrep myRegex filename) filenames

myGrep :: String -> String -> IO ()
myGrep myRegex filename = do
  fileSlurp <- readFile filename
  mapM_ putStrLn $
    filter (=~ myRegex) (lines fileSlurp)
