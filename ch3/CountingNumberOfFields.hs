#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
-}
module CountingNumberOfFields where

import Text.CSV
import Data.List
import Control.Monad.Except

main :: IO ()
main = runActualData

runActualData = do
  result <- parseCSVFromFile "all_week.csv"
  case result of
    Right csv -> print $ lineNumbersWithIncorrectCount csv
    Left err  -> print err 


runTestData = do
  result <- parseCSVFromFile "poorFieldCounts.csv"
  case result of
    Right csv -> print $ lineNumbersWithIncorrectCount csv
    Left err  -> print err 

lineNumbersWithIncorrectCount :: CSV -> [(Integer, Integer)]
lineNumbersWithIncorrectCount (fields:records) =
  filter (\(_, thisCount) -> thisCount /= nfields) lineNoCountPairs
    where 
      nfields = genericLength fields
      count = countFieldsInEachRecord records
      lineNoCountPairs = zip [1..] count

countFieldsInEachRecord :: CSV -> [Integer]
countFieldsInEachRecord csv =
  fmap genericLength (init csv)




