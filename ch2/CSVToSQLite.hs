#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
   --package sqlite
-}

{-# LANGUAGE OverloadedStrings #-}

module CSVToSQLite where

import Text.CSV
import Database.SQL
import Data.List (intercalate)
import Data.Text (Text)

import Debug.Trace

main :: IO ()
main = do
  undefined

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

-- checkSQL :: IO ()
-- checkSQL = do
--   conn <- connectSqlite3 "earthquakes.sql"
--   magnitudes <- quickQuery' conn "SELECT mag FROM oneWeek" []
--   putStrLn $ fromSql $ head $ head magnitudes :: Double

--   let magnitudesDouble = 
--         map (\record ->
--               fromSql $ head record :: Double) magnitues

--   putStrLn $ average magnitudesDouble

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
                -> FilePath
                -> [String]
                -> CSV
                -> IO ()
convertCSVToSQL tableName outFileName fields records =
  -- Check to make sure that the number of columns
  -- matches the number of fields
  if nFieldsInFile == nFieldsInFields then do
    -- Open a connection
    -- conn <- open outFileName

    -- Create a new table
    -- execute conn 
    --         "CREATE TABLE ? (?)" 
    --         (tableName, intercalate ", " fields)

    -- Load contents of CSV file into table
    -- stmt <- prepare conn insertStatement
    -- executeMany stmt 
    --             (tail (filter (\record -> nFieldsInFile == length record) sqlRecords))

    -- -- Commit changes
    -- commit conn
      
    -- -- Close the connection
    -- disconnect conn

    -- -- Report that we were successful
    -- putStrLn "Successful"

    undefined

  else
    putStrLn "The number of input fields differ from the csv file."

  where
    nFieldsInFile = length $ head records
    nFieldsInFields = length fields

    createTable = 
      SQLCreateTable $ Table "oneWeek" tableColumns []
        where 
          makeColumn field =
            let list = words field
                name = head list
                type' = last list

             in Column name type' []

          tableColumns = fmap makeColumn fields

    -- insertStatement =
    --   "INSERT INTO " ++
    --   tableName ++ " VALUES (" ++
    --   (intercalate ", " (replicate nFieldsInFile "?")) ++ ")""?" ++
    --   ")"

    -- sqlRecords = 
    --   fmap (\record ->
    --     fmap (\element -> toSql element) record)  records

     
average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / fromIntegral (length xs)
