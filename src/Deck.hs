module Deck where

import Data.List
import System.Random
import Poker
import Control.Applicative

type Deck = [Card]

deck52 = C <$> [Two .. Ace] <*> suits

-- Shuffles our deck
shuffle :: Deck->State StdGen
shuffle d = do gen <- getStdGen
               let shuffd = sort $ zip (randomRs (0,1) gen :: [Float]) d
               return $ map snd shuffd
               
randomCards :: Int -> Deck -> IO [Card]
randomCards n d = do shuffled <- shuffle d
                     return $ take n shuffled

main = do d <- rhuffle deck52               
          putStrLn $ show d
          
          