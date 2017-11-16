#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
   --package text
   --package direct-sqlite
-}

module CSVToSQLite where

import Text.CSV
import Database.SQLite3
import Data.Text (pack, unpack)
import Data.List (intercalate)
import Text.Read (readMaybe)

import Debug.Trace

main :: IO ()
main = do
  runConversion
  checkSQL

runConversion :: IO ()
runConversion = 
  convertCSVFileToSQL inFileName outFileName tableName fields
    where inFileName  = "all_week.csv"
          outFileName = "earthquakes.sql"
          tableName   = "oneWeek"
          fields      =
            [ "time TEXT"
            , "latitude REAL"
            , "longitude REAL"
            , "depth REAL"
            , "mag REAL"
            , "magType TEXT"
            , "nst INTEGER"
            , "gap REAL"
            , "dmin REAL"
            , "rms REAL"
            , "net REAL"
            , "id TEXT"
            , "updated TEXT"
            , "place TEXT"
            , "type TEXT"
            , "horizontalError REAL"
            , "depthError REAL"
            , "magError REAL"
            , "magNst INTEGER"
            , "status TEXT"
            , "locationSource TEXT"
            , "magSource TEXT"
            ]

checkSQL :: IO ()
checkSQL = do
  db <- open (pack "earthquakes.sql")
  execPrint db (pack "SELECT mag FROM oneWeek")

-- Converts a CSV file to an SQL database file
-- Prints "Successful" if successful,
-- error message otherwise.
convertCSVFileToSQL :: String
                    -> String
                    -> String
                    -> [String]
                    -> IO ()
convertCSVFileToSQL inFileName outFileName tableName fields = do
  -- Open and read the CSV file
  input <- readFile inFileName
  let records = parseCSV inFileName input
  
  -- Check to make sure this is a good CSV file
  either handleCSVError convertTool records
  where 
    convertTool = 
      convertCSVToSQL tableName outFileName fields

    handleCSVError csv = 
      putStrLn "This does not appear to be a CSV file."


-- Converts a CSV expression into an SQL database
-- Returns "Successful" if successful,
-- error message otherwise.
convertCSVToSQL :: String
                -> String
                -> [String]
                -> CSV
                -> IO ()
convertCSVToSQL tableName outFileName fields records =
  -- Check to make sure that the number of columns
  -- matches the number of fields
  if nFieldsInFile == nFieldsInFields then do
    -- Open a connection
    db <- open (pack outFileName)

    -- Create a new table
    exec db (pack createStatement)

    -- Load contents of CSV file into table
    let performMultiple =
          fmap (\rec -> do
            -- Is there must be a better way to do this
            statement <- prepare db (pack prepareInsertStatement)
            bind statement $ convertRecordToSQLData rec fields
            step statement
            finalize statement
          ) records'

    sequence_ performMultiple

    close db

    -- Report that we were successful
    putStrLn "Successful"
  else
    putStrLn "The number of input fields differ from the csv file."

  where
    nFieldsInFile = length $ head records'
    nFieldsInFields = length fields

    createStatement = 
      "CREATE TABLE " ++ tableName ++ " (" ++ 
      (intercalate ", " fields) ++ ")"

    prepareInsertStatement =
      "INSERT INTO " ++ tableName ++ " (" ++
      (intercalate ", " fieldWithoutTypes) ++ ")" ++ " VALUES (" ++
      (intercalate ", " (replicate nFieldsInFields "?")) ++ ")"
        where fieldWithoutTypes = 
                fmap (head . words) fields

    records' = filter (/= [""]) records

convertRecordToSQLData :: Record -> [String] -> [SQLData]
convertRecordToSQLData record fields =
  fmap convertTypeToSQLData zipped
    where 
      types = fmap (last . words) fields
      zipped = zip types record

convertTypeToSQLData :: (String, String) -> SQLData
convertTypeToSQLData ("TEXT", field) = SQLText . pack $ field
convertTypeToSQLData ("REAL", field) = 
  case readMaybe field of
    Just real -> SQLFloat real
    Nothing   -> SQLNull
convertTypeToSQLData ("INTEGER", field) = 
  case readMaybe field of
    Just real -> SQLInteger real
    Nothing   -> SQLNull

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / fromIntegral (length xs)
