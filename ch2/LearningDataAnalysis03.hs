#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
   --package sqlite-simple
-}

module LearningDataAnalysis03 where

import Text.CSV
import Database.SQLite.Simple

main :: IO ()
main = do
  undefined

runConversion :: IO ()
runConversion = 
  convertCSVFileToSQL "all_week.csv" "earthquakes.sql"
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
            ]

checkSQL :: IO ()
checkSQL = do
  conn <- connectSqlite3 "earthquakes.sql"
  magnitudes <- quickQuery' conn "SELECT mag FROM oneWeek" []
  putStrLn $ fromSql $ head $ head magnitudes :: Double

  let magnitudesDouble = 
        map (\record ->
              fromSql $ head record :: Double) magnitues

  putStrLn $ average magnitudesDouble

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
    convertTool = convertCSVToSQL tableName outFileName
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
    conn <- connectSqlite3 outFileName

    -- Create a new table
    run conn createStatement []

    -- Load contents of CSV file into table
    stmt <- prepare conn insertStatement
    executeMany stmt 
                (tail (filter (\record -> nFieldsInFile == length record) sqlRecords))

    -- Commit changes
    commit conn
      
    -- Close the connection
    disconnect conn

    -- Report that we were successful
    putStrLn "Successful"

  else
    putStrLn "The number of input fields differ from the csv file."

  where
    nFieldsInFile = length $ head records
    nFieldsInFields = length fields

    createStatement = 
      "CREATE TABLE " ++
      tableName ++
      " (" ++ (intercalate ", " fields) ++ ")"

    insertStatement =
      "INSERT INTO " ++
      tableName ++ " VALUES (" ++
      (intercalate ", " (replicate nFieldsInFile "?")) ++ ")""?" ++
      ")"

    sqlRecords = 
      fmap (\record ->
        fmap (\element -> toSql element) record)  records

    
average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / fromIntegral (length xs)
