--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |
--Module       : Data.HCard.Card
--Author       : Joe Fredette
--License      : BSD3
--Copyright    : Joe Fredette
--
--Maintainer   : Joe Fredette <jfredett.at.gmail.dot.com>
--Stability    : Unstable
--Portability  : portable 
--
--------------------------------------------------------------------------------
--Description  : A module containing the main "Card" class, a sort of type-indexed
--      record type.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Data.HCard.Card where

import Data.HCard.Misc

-- | The Main class, this is -- effectively -- a type-indexed record. Specifically, it requires 
-- two types, one representing the suit, the other representing the index/rank. The suit, 
-- index, and construct functions are generic forms of the record accessors. 
-- 
-- The bulk of the implementation takes place in generic type instances, supporting equality 
-- irrelevant of ordering, ordering, parsing from a "normal form" (<index>-<suit>) and enum/bounded
--
--
-- TODO: Write deriving instance?
class (Eq s, Eq i, Show s, Show i) => Card s i where
        data CardT s i  :: * 
        suit            :: CardT s i -> s
        index           :: CardT s i -> i
        construct       :: i -> s -> CardT s i
        (@@)            :: i -> s -> CardT s i 
        -- MRD : Everything but @@
        x @@ y = construct x y


instance Card s i => Show (CardT s i) where
        show c = (show . index $ c) ++ "-" ++ (show . suit $ c)

instance Card s i => Eq (CardT s i) where
        ca == cb = (suit ca == suit cb) && (index ca == index cb)

type Cards s i = [CardT s i]

-- | Parse is just a read instance with read exposed as the MRD. For most types, this will
-- just be Read, but when dealing w/ index types, it can be a pain to parse numbers via 
-- read and end up with the expected format of "<index>-<suit>"
class Parse a where
        parse :: String -> a

instance (Parse s, Parse i, Card s i) => Parse (CardT s i) where
        parse s = construct (parse idx) (parse suit)
                where (idx, suit) = (\(x,y) -> (x, tail y)) 
                                  $ break (=='-') s 

instance (Ord s, Ord i, Card s i) => Ord (CardT s i) where
        compare c1 c2 
                | suit c1 < suit c2     = LT
                | suit c1 > suit c2     = GT
                | suit c1 == suit c2    = compare (index c1) (index c2)


instance (Bounded s, Bounded i, Enum s, Enum i, Card s i) => Enum (CardT s i) where
        toEnum i = [construct x y | x <- allBdd, y <- allBdd] !! i    
        fromEnum v = ((1 + fromEnum (suit v)) * (fromEnum (index v))) - 1

instance (Bounded s, Bounded i, Enum s, Enum i, Card s i) => Bounded (CardT s i) where
        minBound = construct minBound minBound
        maxBound = construct maxBound maxBound 





