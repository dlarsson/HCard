--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |
--Module       : Data.HCard.Hand
--Author       : Joe Fredette
--License      : BSD3
--Copyright    : Joe Fredette
--
--Maintainer   : Joe Fredette <jfredett.at.gmail.dot.com>
--Stability    : Unstable
--Portability  : portable 
--
--------------------------------------------------------------------------------
--Description  : Functions relating to a particular dealt hand of cards.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

module Data.HCard.Hand where

import Data.HCard.Card
import Data.HCard.Misc
import Data.List

-- | A type to separate Hands from Decks.
newtype Hand s i = Hand (Cards s i)
        deriving (Show)

instance Card s i => Eq (Hand s i) where
        (Hand h1) == (Hand h2) = (h1 `subset` h2) && (h2 `subset` h1)

instance (Parse s, Parse i, Card s i) => Parse (Hand s i) where
        parse s = Hand $ map parse (words s)

