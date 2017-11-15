#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
-}

{-# LANGUAGE FlexibleContexts #-}

module RegexSearching where

import Data.List
import Text.CSV
import Text.Regex.Posix ((=~))

main :: IO ()
main = do
  eitherCSV <- parseCSVFromFile "poordata.csv"

  putStrLn "Matches field that contains `male`"
  print $ checkCSV (\x -> x =~ "male") eitherCSV
  putStrLn ""

  putStrLn "Matches empty field"
  print $ checkCSV (\x -> x =~ "^$") eitherCSV
  putStrLn ""

  putStrLn "Matches one or more empty space followed by end of string"
  print $ checkCSV (\x -> x =~ "\\s*$") eitherCSV
  putStrLn ""

  putStrLn "Only get the badly formattedbirthdays"
  let birthdayRegex = "^[1-9][0-9]?/[1-9][0-9]?/[12][0-9][0-9][0-9]$"
      func x = not (x =~ birthdayRegex)
      result = 
        either
          Left
          (\csv ->
            Right $ 
              filter
              (\(_, heading, _) -> heading == "Birthday")
              csv
          )
          (checkCSV func eitherCSV)
  print result


checkCSV :: (String -> Bool)
         -> Either a CSV 
         -> Either String [(String, String, String)]
checkCSV func eitherCSV = do
  either (\_ -> Left "CSV Problem")
         (\csv -> identifyInCSV func csv "Number")
         eitherCSV

identifyInCSV ::
     (String -> Bool)
  -> CSV
  -> String
  -> Either String [(String, String, String)]
identifyInCSV myFieldFunc csv idColumn =
  either 
  Left
  (\idColumnIndex -> 
    let headings = head csv
        records  = tail csv
     in Right $ 
        concatMap 
        (\record ->
          identifyMatchingFields 
            myFieldFunc 
            record 
            headings 
            idColumnIndex)
        records)
  (getColumnInCSV csv idColumn)

identifyMatchingFields ::
     (String -> Bool)
  -> [String]
  -> [String]
  -> Integer
  -> [(String, String, String)]
identifyMatchingFields
  myStringCmpFunc record headings idColumnIndex =
     filter
       (\(_, _, field) -> myStringCmpFunc field) keyvalue
     where 
       nfields = length headings
       id = genericIndex record idColumnIndex
       keyvalue =
         zip3 (replicate nfields id) headings record

getColumnInCSV :: CSV -> String -> Either String Integer
getColumnInCSV csv columnName =
  case lookupResponse of
    Nothing -> Left "The column does not exist in this CSV file."
    Just x  -> Right (fromIntegral x)
  where 
    lookupResponse = findIndex (== columnName) (head csv)
