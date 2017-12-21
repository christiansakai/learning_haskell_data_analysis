#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
   --package text
   --package direct-sqlite
   --package erf
-}
module HomeFieldAdvantage where

import Data.List
import Graphics.EasyPlot
import Text.CSV
import Database.SQLite3
import Data.Text (pack, unpack)
import Text.Read (readMaybe)
import Control.Monad (forever)
import Data.Foldable (all)
import Data.Number.Erf

main :: IO ()
main = do
  putStrLn "Done"

standardError :: [Double] -> Double
standardError values =
  standardDeviation values / (sqrt $ genericLength values)

standardDeviation :: [Double] -> Double
standardDeviation values =
  (sqrt . sum $ fmap (\x -> (x - mu) * (x - mu)) values) / sqrt_nm1
    where 
      mu = average values
      sqrt_nm1 = sqrt (genericLength values - 1)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / fromIntegral (length xs)

plotRunsHomeAwayDiff :: IO Bool
plotRunsHomeAwayDiff = do
  homeAwayDiff <- runsHomeAwayDiff

  plot (PNG "HomeScoreAwayScoreDiff.png") $
    Data2D [Title "Difference in Runs at Home (x axis) and Runs Away (y axis)"] [] $ 
      zip [1..] homeAwayDiff

runsHomeAwayDiff :: IO [Double]
runsHomeAwayDiff = do
  homeAway <- runsHomeAway
  return $ fmap (\(a, b) -> a - b) homeAway

plotRunsHomeAway :: IO Bool
plotRunsHomeAway = do
  homeAway <- runsHomeAway

  plot (PNG "HomeScoreAwayScore.png") $
    Data2D [Title "Runs at Home (x axis) and Runs Away (y axis)"] [] homeAway

runsHomeAway :: IO [(Double, Double)]
runsHomeAway = do
  home <- runsAtHome
  away <- runsAway

  return $ zip (readDoubleColumn home 1) (readDoubleColumn away 1)

runCheckScore :: IO [[SQLData]]
runCheckScore =
  queryDatabase "winloss.sql" "SELECT SUM(awayscore), SUM(homescore) FROM winloss"

runsAtHome :: IO [[SQLData]]
runsAtHome =
  queryDatabase 
    "winloss.sql" 
    ("SELECT hometeam, SUM(homescore) FROM " ++
     "winloss GROUP BY hometeam ORDER BY hometeam")

runsAway :: IO [[SQLData]]
runsAway =
  queryDatabase
    "winloss.sql"
    ("SELECT awayteam, SUM(awayscore) FROM " ++
     "winloss GROUP BY awayteam ORDER BY awayteam")

queryDatabase :: FilePath -> String -> IO [[SQLData]]
queryDatabase databaseFile query = do
  db <- open (pack databaseFile)
  statement <- prepare db (pack query)

  let queryRecursive :: IO [[SQLData]]
      queryRecursive = do
        step statement
        -- result :: [SQLData]
        result <- columns statement
        if isEndOfQuery result then
          return []
        else do
          -- subResult :: [SQLData]
          subResult <- queryRecursive 
          return $ [result] ++ subResult

  result <- queryRecursive

  finalize statement
  close db

  return result

  where
    isEndOfQuery result = all (== SQLNull) result

readIntegerColumn :: [[SQLData]] -> Integer -> [Integer]
readIntegerColumn sqlData index =
  fmap 
  (\row ->
    let cell = genericIndex row index
     in case cell of
          SQLInteger int -> fromIntegral int
          _ -> 0
  ) 
  sqlData

readDoubleColumn :: [[SQLData]] -> Integer -> [Double]
readDoubleColumn sqlData index = 
  fmap 
  (\row -> 
    let cell = genericIndex row index
     in case cell of 
          SQLFloat float -> float
          SQLInteger int -> fromIntegral int
          _ -> 0.0
  )
  sqlData

readStringColumn :: [[SQLData]] -> Integer -> [String]
readStringColumn sqlData index =
  fmap
  (\row -> 
    let cell = genericIndex row index
     in case cell of
          SQLText text -> (unpack text)
          _ -> ""
  )
  sqlData





-- Functions below this line
-- is to put the data into SQLite3

runConvertGL :: IO ()
runConvertGL =
  convertCSVFileToSQL 
    "winloss2017.csv"
    "winloss.sql"
    "winloss"
    [ "date TEXT"
    , "awayteam TEXT"
    , "hometeam TEXT"
    , "awayscore INTEGER"
    , "homescore INTEGER"
    ]

convertCSVFileToSQL :: String
                    -> String
                    -> String
                    -> [String]
                    -> IO ()
convertCSVFileToSQL inFileName outFileName tableName fields = do
  input <- readFile inFileName
  let records = parseCSV inFileName input
  either handleCSVError convertTool records
  where 
    convertTool = 
      convertCSVToSQL tableName outFileName fields

    handleCSVError csv = 
      putStrLn "This does not appear to be a CSV file."

convertCSVToSQL :: String
                -> String
                -> [String]
                -> CSV
                -> IO ()
convertCSVToSQL tableName outFileName fields records =
  if nFieldsInFile == nFieldsInFields then do
    db <- open (pack outFileName)

    exec db (pack createStatement)

    let performMultiple =
          fmap (\rec -> do
            statement <- prepare db (pack prepareInsertStatement)
            bind statement $ convertRecordToSQLData rec fields
            step statement
            finalize statement
          ) records'

    sequence_ performMultiple
    close db
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

    records' = filter (/= [""]) (tail records)

convertRecordToSQLData :: Record -> [String] -> [SQLData]
convertRecordToSQLData record fields =
  fmap convertTypeToSQLData zipped
    where 
      types = fmap (last . words) fields
      zipped = zip types record

convertTypeToSQLData :: (String, String) -> SQLData
convertTypeToSQLData ("STRING", field) = SQLText . pack $ field
convertTypeToSQLData ("TEXT", field) = SQLText . pack $ field
convertTypeToSQLData ("REAL", field) = 
  case readMaybe field of
    Just real -> SQLFloat real
    Nothing   -> SQLNull
convertTypeToSQLData ("INTEGER", field) = 
  case readMaybe field of
    Just real -> SQLInteger real
    Nothing   -> SQLNull



