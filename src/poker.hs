module Poker where

import Data.List
import Control.Applicative
import Data.Function

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine |
              Ten | Jack | Queen | King | Ace deriving
              (Show,Eq,Enum,Bounded,Ord)

data Card = C {rank :: Rank, suit :: Suit} deriving (Eq)

data Hand = HighCard [Rank] | Pair Rank [Rank] | 
            TwoPair Rank Rank [Rank] | ThreeOfKind Rank [Rank] | 
            Straight Rank | Flush [Rank] | FullHouse Rank Rank | 
            FourOfKind Rank [Rank]| StraightFlush Rank deriving (Eq, Show, Ord)

type Cards = [Card]

-- Simple list containing each of the possible suits
suits :: [Suit]
suits = [Spades,Hearts,Diamonds,Clubs]

-- A standard fifty two card deck..
deck :: Cards
deck = C <$> [Two .. Ace] <*> suits

instance Ord Card where
  (C v s) `compare` (C v' s') = v `compare` v'

instance Show Card where
  show (C v s) = show v ++ " of " ++ show s
  
beats :: Cards -> Cards -> Bool
cs `beats` cs' = (handStd cs) > (handStd cs')

-- Computes the type of hand for a standard 5 card poker hand
handStd :: Cards -> Hand
handStd cs@(y:ys) = hand' profile cs 
  where profile = (map length) grouped
        grouped = reverse $ sortBy (compare `on` (\x->(length x,x))) cs'
          where cs' =  (group . map rank . sort) cs
        
        
        hand' [1,1,1,1,1] xs
          | isStraight xs && isFlush xs = StraightFlush (straightRank grouped)
          | isFlush xs = Flush (concat grouped)
          | isStraight xs = Straight (straightRank grouped)
          | otherwise = HighCard (concat grouped)
        hand' [2,1,1,1] xs = Pair ((head . head) grouped) (concat $ tail grouped)
        hand' [2,2,1] xs = TwoPair (head (grouped !! 0)) (head (grouped !! 1)) (last grouped)
        hand' [3,2] xs = FullHouse (head (grouped !! 0)) (head (grouped !! 1))
        hand' [4,1] xs = FourOfKind (head $ head grouped) (last grouped)
        hand' [3,1,1] xs = ThreeOfKind (head $ head grouped) (last grouped)
        hand' _ xs = error "Input required to be exactly 5 cards"
        
        isFlush = (==1) . length . nub . map suit
        isStraight (y:ys) | b == Ace = bs == [Two .. Five]
                          | otherwise = (b:bs) == take 5 [b ..]
          where (b:bs) = map rank $ sort (y:ys)
        
        straightRank gs = case  g' of
          Ace -> if head gs' == Five then Five else Ace
          otherwise -> g'
          where (g':gs') = concat gs
