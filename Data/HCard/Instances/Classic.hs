--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |
--Module       : Data.HCard.Instances.Classic
--Author       : Joe Fredette
--License      : BSD3
--Copyright    : Joe Fredette
--
--Maintainer   : Joe Fredette <jfredett.at.gmail.dot.com>
--Stability    : Unstable
--Portability  : portable
--
--------------------------------------------------------------------------------
--Description  : Describes the "French Deck" set of playing cards, the most 
-- common deck in the US 
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Data.HCard.Instances.Classic where

import Data.HCard.Card
import Data.HCard.Misc
import Data.HCard.Deck
import Data.HCard.Hand


-- | The Suits of the so-called "French" deck, the most common American deck of 
-- cards.
data Suit  = H | D | C | S
        deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | The Indices of the french deck
data Index = Ace | Jack | Queen | King | V Int
        deriving (Eq)

instance Show (Index) where
        show i = case i of 
                Ace   -> "A"
                Jack  -> "J"
                Queen -> "Q"
                King  -> "K"
                V i   -> show i

instance Ord Index where
        compare i j = case i of
                Ace   -> if j == Ace then EQ else LT
                King  -> if j == King then EQ else GT
                Queen -> case j of
                        Queen -> EQ
                        King  -> LT
                        _     -> GT     
                Jack  -> case j of
                        Jack  -> EQ
                        Queen -> LT
                        King  -> LT
                        _     -> GT
                (V x) -> case j of
                        (V y) -> compare x y
                        Ace   -> LT
                        _     -> GT       

instance Enum Index where
        toEnum i 
             | i >= 2 && i <= 10 = V i
             | otherwise         = case i of
                                        1  -> Ace
                                        11 -> Jack
                                        12 -> Queen
                                        13 -> King
                                        _  -> error $ "No enum for " ++ show i ++ "."
        fromEnum i = case i of
                Ace   -> 1
                Jack  -> 11
                Queen -> 12
                King  -> 13
                V x   -> x
       
instance Bounded Index where
        minBound = Ace
        maxBound = King


 
instance Parse Suit where
        parse = read

instance Parse Index where
        parse i = case i of
                "A" -> Ace
                "J" -> Jack
                "Q" -> Queen
                "K" -> King
                i   -> if (ri > 10 || ri < 2)
                       then error "Valid Number cards are 2-10" 
                       else V ri 
                where ri = read i :: Int
        


instance Card Suit Index where
        data CardT Suit Index = Classic Suit Index
        suit  (Classic s _) = s
        index (Classic _ i) = i
        construct i s = Classic s i

-- | Type synonyms to make using the polymorphic bits easier
type Classic = CardT Suit Index   
type ClassicDeck = Deck Suit Index
type ClassicDeckST = DeckST Suit Index
type ClassicHand = Hand Suit Index

-- | Wrapper which forces the polymorphic dealHands to work with French-deck cards only.
deal :: Int -> Int -> ClassicDeckST [ClassicHand]
deal x y = dealHands x y


