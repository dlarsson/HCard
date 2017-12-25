--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |
--Module       : Data.HCard.Deck
--Author       : Joe Fredette
--License      : BSD3
--Copyright    : Joe Fredette
--
--Maintainer   : Joe Fredette <jfredett.at.gmail.dot.com>
--Stability    : Unstable
--Portability  : portable 
--
--------------------------------------------------------------------------------
--Description  : Functions relating to a Deck of cards, eg shuffling, dealing,
--      etc.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

module Data.HCard.Deck where

import System.Random
import System.Random.Shuffle

import Control.Monad.State

import Data.HCard.Misc
import Data.HCard.Card
import Data.HCard.Hand


-- | Separate Deck from Hand, even though the types are isomorphic, we don't want shuffling to 
-- be to liberal.
--
-- TODO: Make Deck clever enough to support reshuffling when the deck runs out -- it should store cards it has 
--  already seen till it runs out of the main deck, reshuffle, redeal.
newtype Deck s i = Deck (Cards s i)
        deriving (Show)

-- | Type wrapper for stateful decks, useful for sorting
type DeckST s i = State (Deck s i)


instance Card s i => Eq (Deck s i) where
        (Deck h1) == (Deck h2) = (h1 `subset` h2) && (h2 `subset` h1)

-- | Creates a deck, used as in: `mkDeck::<your deck type here>`, or w/ inference.
mkDeck :: (Bounded s, Bounded i, Enum s, Enum i, Card s i) => Deck s i 
mkDeck = Deck $ enumFromTo minBound maxBound 

-- | Shuffles a deck given a generator
shuffleDeck :: (Card s i, RandomGen g) => Deck s i -> g -> Deck s i 
shuffleDeck (Deck ds) g = Deck (shuffle' ds (length ds) g)

-- | Shuffles using the standard generator
shuffleDeckIO :: Card s i => Deck s i -> IO (Deck s i)
shuffleDeckIO (Deck ds) = do
                gen <- getStdGen
                return $ Deck (shuffle' ds (length ds) gen)

-- | Deals `n` hands of `qty` cards, written in the state monad.
dealHands :: Card s i => Int -> Int -> (DeckST s i) [Hand s i]
dealHands 0   _   = return []
dealHands _   0   = error "Can't deal zero cards."
dealHands num qty = do 
                (Deck deck) <- get
                if (length deck < qty) then error "Not enough cards in deck"
                                       else do { hand <- dealHand qty
                                               ; next <- dealHands (num - 1) qty
                                               ; return $ hand : next
                                               }
-- | Helper for dealHands, also somewhat useful, equiv. to `dealHands 1 qty`
dealHand :: Card s i => Int -> (DeckST s i) (Hand s i)
dealHand qty = do
        (Deck deck) <- get       
        let (hand, rem) = splitAt qty deck
        put (Deck rem)
        return $ Hand hand





