#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package csv
-}

module LearningDataAnalysis02 where

import Data.List
import Data.Either
import Text.CSV
import Control.Monad.Except

import Debug.Trace

main :: IO ()
main = do
  result <- applyToColumnInCSVFile (average . readColumn) "all_week.csv" "mag"
  putStrLn . show $ result

type Error = String

makeIntoExcepT :: ([String] -> b)
               -> FilePath
               -> String
               ->ExceptT Error IO b
makeIntoExcepT func filePath columnName = 
  ExceptT $ applyToColumnInCSVFile func filePath columnName

applyToColumnInCSVFile :: ([String] -> b)
                       -> FilePath
                       -> String
                       -> IO (Either Error b)
applyToColumnInCSVFile func filePath columnName = do
  csv <- parseCSVFromFile filePath
  return $ either
    handleCSVError
    (\csv -> applyToColumnInCSV func csv columnName)
    csv
  where 
    handleCSVError _ = 
      Left "This does not appear to be a CSV file."

applyToColumnInCSV :: ([String] -> b) 
                   -> CSV
                   -> String
                   -> Either Error b
applyToColumnInCSV func csv columnName =
  either Left (Right . func . filter (/= "") . elements) columnIndex
    where 
      columnIndex = getColumnInCSV csv columnName
      nFieldsInFile = length . head $ csv
      records = tail $ filter (\record -> nFieldsInFile == length record) csv
      elements columnIndex = fmap (\record -> genericIndex record columnIndex) records


getColumnInCSV :: CSV -> String -> Either Error Integer
getColumnInCSV csv columnName =
  case lookupResponse of
    Nothing -> Left "The column does not exist in this CSV file."
    Just x  -> Right (fromIntegral x)
  where 
    lookupResponse = findIndex (== columnName) (head csv)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / fromIntegral (length xs)

readColumn :: [String] -> [Double]
readColumn column = fmap read column

