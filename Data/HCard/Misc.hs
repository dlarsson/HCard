--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- |
--Module       : Data.HCard.Misc
--Author       : Joe Fredette
--License      : BSD3
--Copyright    : Joe Fredette
--
--Maintainer   : Joe Fredette <jfredett.at.gmail.dot.com>
--Stability    : Unstable
--Portability  : portable 
--
--------------------------------------------------------------------------------
--Description  : Miscellaneous helper functions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

module Data.HCard.Misc where

import Data.List



subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True 
subset (x:xs) ls = (x `elem` ls) && (subset xs (ls \\ [x]))

allBdd :: (Enum a, Bounded a) => [a]
allBdd = enumFromTo minBound maxBound
