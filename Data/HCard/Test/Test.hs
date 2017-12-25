--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |
--Module       : Test
--Author       : Joe Fredette
--License      : BSD3
--Copyright    : Joe Fredette
--
--Maintainer   : Joe Fredette <jfredett.at.gmail.dot.com>
--Stability    : Unstable
--Portability  : portable 
--
--------------------------------------------------------------------------------
--Description  : Tests for HCard
--
-- TODO: This is an anaemic set of tests, flesh it out!
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

module Data.HCard.Test.Test where

import Test.QuickCheck

import Data.HCard
import Data.HCard.Instances
import Data.HCard.Misc

import System.Random

import Control.Monad.State


-- conf = Config 1000 0 (configSize defaultConfig) (configEvery defaultConfig)

instance (Arbitrary s, Arbitrary i, Card s i) => Arbitrary (CardT s i) where
        arbitrary = do 
                x <- arbitrary
                y <- arbitrary
                return $ construct x y 

instance (Arbitrary s, Arbitrary i, Card s i) => Arbitrary (Deck s i) where
        arbitrary = do
                x <- arbitrary
                return $ Deck x

instance Arbitrary Suit where
        arbitrary = elements [H,S,C,D]       

instance Arbitrary Index where
        arbitrary = elements ([Ace,King,Queen,Jack] ++ map V [2..10]) 

instance Arbitrary StdGen where
        arbitrary = do
                x <- arbitrary
                return (mkStdGen x)

-- Tests

---------------------
-- Deck/Hand tests --
---------------------
classic_shuffle_eq :: StdGen -> Bool
classic_shuffle_eq g = shuffleDeck (mkDeck::ClassicDeck) g == mkDeck

classic_deal_subset_deck :: StdGen -> Int -> Int -> Property
classic_deal_subset_deck g n q = (n >= 1 && q >= 1 && n <= len `div` q) 
                             ==> (all (\(Hand x) -> x `subset` shuffled) hands)
                where (shuffled, len) = (\(Deck d) -> (d, length d)) $ shuffleDeck (mkDeck::ClassicDeck) g
                      (hands, _) = runState (dealHands n q) (Deck shuffled)

classic_deal_union_id :: StdGen -> Int -> Int -> Property
classic_deal_union_id g n q = (n >= 1 && q >= 1 && n <= len `div` q)
                          ==> undealt == shuffled
                where (shuffled, len) = (\(Deck d) -> (d, length d)) $ shuffleDeck (mkDeck::ClassicDeck) g
                      undealt = uncurry undeal $ runState (dealHands n q) (Deck shuffled)
                      undeal hs (Deck d) = ((concatMap (\(Hand h) -> h) hs) ++ d)
                      

deck_hand_tests = [ quickCheck classic_shuffle_eq
                  , quickCheck classic_deal_subset_deck
                  , quickCheck classic_deal_union_id]

----------------------
-- Parse/Show tests --
----------------------
show_parse_id_classic :: Classic -> Bool
show_parse_id_classic a = (parse $ show a) == a


parse_show_tests = [quickCheck show_parse_id_classic]

----------------
-- Misc tests --
----------------
subset_prop :: [Int] -> [Int] -> Bool
subset_prop ls as = ls `subset` (ls ++ as)

subset_eq :: [Int] -> Bool
subset_eq ls = ls `subset` ls

misc_tests = [quickCheck subset_prop, quickCheck subset_eq]


-- Run tests:
test = sequence_ $ concat [ misc_tests 
                          , deck_hand_tests
                          , parse_show_tests
                          ]

