#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
-}

module LearningDataAnalysis01 where

import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  values <- getArgs
  print . median $ fmap read values

median :: [Double] -> Double
median [] = 0
median xs =
  if oddInLength then
    middleValue
  else
    (middleValue + beforeMiddleValue) / 2
  where 
    sortedList = sort xs
    oddInLength = 1 == mod (genericLength xs) 2
    middle = floor $ (genericLength xs) / 2
    middleValue = genericIndex sortedList middle
    beforeMiddleValue = genericIndex sortedList (middle - 1)

vowelIndices :: String -> [Integer]
vowelIndices word =
  fmap fst $ filter inVowel $ zipped
    where inVowel (_, letter) = elem letter "aeiouAEIOU"
          zipped = zip [1..] word

