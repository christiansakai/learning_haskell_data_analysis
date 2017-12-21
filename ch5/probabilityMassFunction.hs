#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --package exact-combinatorics
-}
module ProbabilityMassFunction where

import Math.Combinatorics.Exact.Binomial
import Graphics.EasyPlot
import System.Random

main :: IO ()
main = do
  g <- newStdGen

  let coinflips :: [Int]
      coinflips = take 1000 $ randomRs (0, 1) g

  print $ sum coinflips

getRandoms :: RandomGen g => g -> [Double]
getRandoms g = take 3 $ randoms g

getRandomInts :: RandomGen g => g -> [Int]
getRandomInts g = take 5 $ randomRs (0, 100) g
  
plot2D :: IO Bool
plot2D = do
  plot (PNG "coinflips.png") $ Function2D
         [Title  "Coin Flip Probabilities"]
         [Range 0 1000]
         (\k -> probabilityMassFunction (floor k) 1000 0.5)

probabilityMassFunction :: Integral a
                        => a
                        -> a
                        -> Double
                        -> Double
probabilityMassFunction k n p =
  (fromIntegral (n `choose` k)) * (p^k) * ((1 - p)^(n - k))


