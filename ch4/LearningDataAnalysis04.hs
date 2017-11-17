#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
   --package easyplot
   --package text
   --package direct-sqlite
-}
module LearningDataAnalysis04 where

import Data.List
import Graphics.EasyPlot
import Text.CSV
import Database.SQLite3
import Data.Text (pack, unpack)
import Text.Read (readMaybe)
import Control.Monad (forever)
import Data.Foldable (all)

main :: IO ()
main = do
  putStrLn "Done"

plotEarthquake :: IO Bool
plotEarthquake = do
  coords <- pullLatituteLongitude "earthquakes.sql" "oneMonth"
  plot (PNG "earthquakes.png")
    [ Data2D
      [ Title "Earthquakes"
      , Color Red
      , Style Dots
      ]
      []
      coords
    ]

pullLatituteLongitude :: String -> String -> IO [(Double, Double)]
pullLatituteLongitude databaseFile tableName = do
  result <- queryDatabase
    databaseFile ("SELECT latitude, longitude FROM " ++ tableName)

  return $ zip
    (readDoubleColumn result 1)
    (readDoubleColumn result 0)

runConvertEarthquake :: IO ()
runConvertEarthquake =
  convertCSVFileToSQL 
    "all_month.csv" 
    "earthquakes.sql" 
    "oneMonth"
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

plotAppleMovingAverage :: IO Bool
plotAppleMovingAverage = do
  aapl <- pullStockClosingPrices "aapl.sql" "aapl"
  let aapl252 = take 252 aapl
      aapl252Pc = applyPercentChangeToData aapl252
      aapl252Ma20 = applyMovingAverageToData aapl252Pc 20

  plot (PNG "aapl_20dayma.png") $ 
    [ Data2D 
      [ Title "AAPL - One Year, % Change"
      , Style Lines
      , Color Red
      ]
      []
      aapl252Pc
    , Data2D
      [ Title "AAPL 20-Day M"
      , Style Lines
      , Color Black
      ]
      []
      aapl252Ma20
    ]

applyMovingAverageToData :: [(Double, Double)] 
                         -> Integer
                         -> [(Double, Double)]
applyMovingAverageToData dataset window =
  zip [(fromIntegral window)..] 
      (movingAverage (fmap snd (reverse dataset)) window)

movingAverage :: [Double] -> Integer -> [Double]
movingAverage values window
  | window >= genericLength values = [average values]
  | otherwise =
    let headValues = genericTake window values
        tailAverages = movingAverage (tail values) window
     in (average headValues) : tailAverages

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / fromIntegral (length xs)

plotAppleMicrosoftGoogle :: IO Bool
plotAppleMicrosoftGoogle = do
  aapl <- pullStockClosingPrices "aapl.sql" "aapl"
  let aapl252 = take 252 aapl
      aapl252Pc = applyPercentChangeToData aapl252

  googl <- pullStockClosingPrices "googl.sql" "googl"
  let googl252 = take 252 googl
      googl252Pc = applyPercentChangeToData googl252

  msft <- pullStockClosingPrices "msft.sql" "msft"
  let msft252 = take 252 msft
      msft252Pc = applyPercentChangeToData msft252

  plot (PNG "aapl_googl_msft_pc.png") $ 
    [ Data2D 
      [ Title "AAPL - One Year, % Change"
      , Style Lines
      , Color Red
      ]
      []
      aapl252Pc
    , Data2D
      [ Title "GOOGL - One Year, % Change"
      , Style Lines
      , Color Blue
      ]
      []
      googl252Pc
    , Data2D
      [ Title "MSFT - One Year, % Change"
      , Style Lines
      , Color Green
      ]
      []
      msft252Pc
    ]

runConvertGoogle :: IO ()
runConvertGoogle =
  convertCSVFileToSQL 
    "googl.csv" 
    "googl.sql" 
    "googl"
    [ "date STRING"
    , "open REAL"
    , "high REAL"
    , "low REAL"
    , "close REAL"
    , "volume REAL"
    , "adjclose REAL"
    ]

runConvertMicrosoft :: IO ()
runConvertMicrosoft = 
  convertCSVFileToSQL
    "msft.csv"
    "msft.sql"
    "msft"
    [ "date STRING"
    , "open REAL"
    , "high REAL"
    , "low REAL"
    , "close REAL"
    , "volume REAL"
    , "adjclose REAL"
    ]


plotPercentChange :: IO Bool
plotPercentChange = do
  aapl <- pullStockClosingPrices "aapl.sql" "aapl"

  let aapl252 = take 252 aapl
      aapl252Pc = applyPercentChangeToData aapl252

  plot (PNG "aapl_oneyear_pc.png") $ 
    Data2D [Title "AAPL - One Year, % Change", Style Lines] [] $ aapl252Pc

applyPercentChangeToData :: [(Double, Double)]
                         -> [(Double, Double)]
applyPercentChangeToData dataset =
  zip indices scaledData
    where 
      (_, first) = last dataset
      indices = reverse [1.0..(genericLength dataset)]
      scaledData =
        fmap (\(_, value) -> percentChange value first) dataset

percentChange :: Double -> Double -> Double
percentChange value first =
  100.0 * (value - first) / first

plotLastYearLine :: IO Bool
plotLastYearLine = do
  aapl <- pullStockClosingPrices "aapl.sql" "aapl"
  plot (PNG "aapl_oneyear.png") $ 
    Data2D [Title "AAPL", Style Lines] [] $ take 252 aapl

plotStandard :: IO Bool
plotStandard = do
  aapl <- pullStockClosingPrices "aapl.sql" "aapl"
  plot (PNG "aapl.png") $ 
    Data2D [Title "AAPL"] [] $ aapl

plotLine :: IO Bool
plotLine = do
  aapl <- pullStockClosingPrices "aapl.sql" "aapl"
  plot (PNG "aapl_line.png") $ 
    Data2D [Title "AAPL", Style Lines] [] $ aapl

pullStockClosingPrices :: FilePath
                       -> String
                       -> IO [(Double, Double)]
pullStockClosingPrices databaseFile tableName = do
  result <- queryDatabase
    databaseFile ("SELECT rowId, adjclose FROM " ++ tableName)

  return $ zip
    (reverse $ readDoubleColumn result 0)
    (readDoubleColumn result 1)

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


runConvertAAPL :: IO ()
runConvertAAPL =
  convertCSVFileToSQL 
    "aapl.csv"
    "aapl.sql"
    "aapl"
    [ "date STRING"
    , "open REAL"
    , "high REAL"
    , "low REAL"
    , "close REAL"
    , "volume REAL"
    , "adjclose REAL"
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
