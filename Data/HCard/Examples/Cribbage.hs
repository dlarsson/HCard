--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |
--Module       : Cribbage
--Author       : Joe Fredette
--License      : BSD3
--Copyright    : Joe Fredette
--
--Maintainer   : Joe Fredette <jfredett.at.gmail.dot.com>
--Stability    : Unstable
--Portability  : portable
--
--------------------------------------------------------------------------------
--Description  : A simple cribbage score counter (minus the "his heels" and 
-- "nobs" rules). Example of how to use the library.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Data.HCard.Examples.Cribbage where

import Data.HCard
import Data.HCard.Instances
import Data.List
import Data.Function


score h = cribbageScore cut hand
        where (cut, hand) = (\(Hand x) -> (head x, Hand . tail $ x)) ((parse h)::ClassicHand)

cribbageScore :: Classic -> ClassicHand -> Int
cribbageScore cut hand = sum $ map (\f -> f cut hand)
                       [ countFifteens 
                       , countPairs   
                       , countRuns     
                       , countFlush
                       --, countHeels 
                       ]



toValue :: Classic -> Int
toValue c = case index c of
        Ace  -> 1
        V x  -> x
        _    -> 10


-- countFifteens, countPairs, countRuns, countFlush, countHeels :: Classic -> ClassicHand -> [Int]
countFifteens cut (Hand hand) = 2 * (length $ filter (==15) $ map valSum extHand)
                        where extHand = allKTups $ hand ++ [cut]
                              valSum xs = sum $ map toValue xs  

countPairs cut (Hand hand) = 2 * (length $ filter isPair extHand)
                where extHand = uniqPairs $ hand ++ [cut] 
                      isPair (x,y) = index x == index y

countRuns cut (Hand hand) = sum 
                          $ map length 
                          $ filter (\x -> length x >= 3) 
                          $ filter isRun 
                          $ map (map toValue) extHand 
        where extHand = map (sortBy (compare `on` index)) $ allKTups (hand ++ [cut]) 
                                      
countFlush cut (Hand hand) = getMax $ filter (>=4) $ map (length . (\(Hand h) -> h)) $ filterSuits extHand
        where extHand = Hand $ hand ++ [cut]
              getMax [] = 0
              getMax ls = maximum ls


countHeels cut (Hand hand) = case index cut of
        Jack    -> if (suit cut) `elem` (map suit hand) then 1 else 0
        _       -> if Jack `elem` suited then 2 else 0
                where suited = map index $ filter (\x -> suit x == suit cut) hand 



isRun []  = True
isRun [x] = True
isRun (x:y:xs) = (abs $ x - y) == 1 && isRun (y:xs)

hand1 = (parse "5-H 5-S 6-D 7-S" ) :: ClassicHand 
hand2 = (parse "5-H 6-H 7-S 10-H") :: ClassicHand
cut = parse "Q-H" :: Classic

filterSuits :: ClassicHand -> [ClassicHand]
filterSuits (Hand hand) = map Hand $ groupBy matchSuit (sort hand)
                where matchSuit c1 c2 = suit c1 == suit c2

allKTups :: [a] -> [[a]]
allKTups []     = []
allKTups (x:xs) = ([x] : (map (x:) (allKTups xs))) ++ allKTups xs

uniqPairs :: Eq a => [a] -> [(a,a)]
uniqPairs xs = map (\(x:y:_) -> (x,y))
             $ filter (\x -> length x == 2) (allKTups xs)
