module Poker where

import Data.List
import Control.Applicative

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine |
              Ten | Jack | Queen | King | Ace deriving
              (Show,Eq,Enum,Bounded,Ord)

data Card = C {rank :: Rank, suit :: Suit} deriving (Eq)

data Hand = HighCard | Pair | TwoPair | ThreeOfKind | Straight | Flush
          | FullHouse | FourOfKind | StraightFlush deriving (Eq, Show, Ord)

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
  
-- Computes the type of hand for a standard 5 card poker hand
hand :: Cards -> Hand
hand cs@(y:ys) = hand' profile cs 
  where profile = (sort . map length . group . map rank . sort) cs
        
        hand' [1,1,1,1,1] xs
          | isStraight xs && isFlush xs = StraightFlush
          | isFlush xs = Flush
          | isStraight xs = Straight
          | otherwise = HighCard
        hand' [1,1,1,2] xs = Pair
        hand' [1,2,2] xs = TwoPair
        hand' [2,3] xs = FullHouse
        hand' [1,4] xs = FourOfKind
        hand' [1,1,3] xs = ThreeOfKind
        hand' _ xs = error "Input required to be exactly 5 cards"
        
        isFlush = (==1) . length . nub . map suit
        isStraight (y:ys) | b == Ace = bs == [Two .. Five]
                          | otherwise = (b:bs) == take 5 [b ..]
          where (b:bs) = map rank (y:ys)
                                   
