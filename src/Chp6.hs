{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chp6 where


import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Monoid (Sum(..))
import Data.Ord (comparing)

import Data.Maybe
import Data.Char (isAlpha)
{-
Note:

(^.)  ::  s -> Lens' s a -> a
(^..) :: s -> Fold s a -> [a]

folded :: Foldable f => Fold (f a) a
folding :: Foldable f => (s -> f a) -> Fold s a
to :: (s -> a) -> Fold s a

---------------
1. 

tcs :: [(Integer, String)]
tcs = zip [1..] ["a", "b", "c"]
tcs ^.. folded

-- tcs ^.. folded . folded
-- -- ["a","b","c"]

-- tcs ^.. folded . folded .folded
-- -- "abc"

-- ["a", "b", "c" ] ^.. folded . folded
-- -- "abc"

-- tcs ^.. folded . _2
-- -- ["a","b","c"]

-- tcs ^.. folded . _1
-- -- [1,2,3]

---------------------------------------------------
-- toListOf (folded . folded) [ [1 .. 3], [4..6] ]

-- toListOf 
--     (folded . folded)
--     (M.fromList [("1", "a"), ("2", "b")])
-- -- "ab"


-- ["bob", "otto", "hannah"] ^.. folded . to reverse 


>>> ("1", "2", "3") ^.. each
["1","2","3"]

>>> ("1", "2", "3") ^.. each . each 
"123"



>>> [ [1 .. 3], [4..6] ] ^.. folded . folding (take 2 )
[1,2,4,5]



("abc", "def") ^..
    folding (\(a,b) -> [a,b])
    . to reverse
    . folded)



-- -- tupz = ("a", (("b", "c"), "d"))
-- -- view (_2 . _1 . _2) tupz == "c"


2. Fill in the blank with a path of folds which results in the specified answer

>>> [1..5] ^.. folded . to (*100)
[100,200,300,400,500]

>>> (1, 2) ^.. both
[1,2]

>>> [(1, "one"), (2, "two")] ^.. folded . folded
["one","two"]

>>> (Just 1, Just 2, Just 3) ^.. each . folded
[1,2,3]

>>> [Left 1, Right 2, Left 3] ^.. folded . folded
[2]


>>> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . both
[[1,2],[3,4],[5,6],[7,8]]

>>> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . both . folded
[1,2,3,4,5,6,7,8]


>>> [1 .. 4] ^.. folded . to (\i -> if i `mod` 2 == 0 then Right i else Left i)
[Left 1,Right 2,Left 3,Right 4]

--- **
>>> [(1, (2, 3)), (4, (5, 6))] ^.. folded . folding (\tup -> tup ^.. _1 <> (tup ^.. _2 . both ))
[1,2,3,4,5,6]

--- NOTE: The cleaner way to do this, from the answers: use (\(a, (b, c)) -> [a, b, c])

-- instructive: note, again, the monoidal mappending with the final folded
>>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folded 
[Left "one",Right 2]
>>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folded . folded
[2]

>>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . _1 . folded
[1]

>>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\tup -> tup ^.. _1 )
[Just 1,Nothing]
>>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\tup -> tup ^.. _1 . folded)
[1]
>>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\tup -> tup ^. _1 )
[1]

-- [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\tup -> tup ^.. _1  <> tup ^.. _2 ) -- type mismatch

--- **
>>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\tup -> tup ^.. _1 . folded <> tup ^.. _2 . folded)
[1,2]

----- NOTE: the nicer way from the answers: use as fn (\(a, b) -> a ^.. folded <> b ^.. folded)

>>> [(1, "one"), (2, "two")] ^.. folded . folding (\tup -> tup ^.. _1 . to Left)
[Left 1,Left 2]


>>> S.fromList ["apricots", "apples"] ^.. folded . to reverse . folded
"selppastocirpa"

---------- 6.3: Combining fold results 

1. Pick the matching action from the list for each example

>>> has folded []
False

>>> foldOf both ("Yo", "Adrian!")
"YoAdrian!"

>>> elemOf each "phone" ("E.T.", "phone", "home")
True

>>> minimumOf folded [5, 7, 2, 3, 13, 17, 11]
Just 2

>>> lastOf folded [5, 7, 2, 3, 13, 17, 11]
Just 11

>>> anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
True

>>> findOf folded even [11, 22, 3, 5, 6]
Just 22

>>> findOf folded (\x -> x == reverse x) ["umbrella", "olives", "racecar", "hammer"]
Just "racecar"

>>> allOf each even (2, 4, 6)
True

Find the pair with the largest integer
>>> maximumByOf each (comparingOf _1) pairs
Just (3,"Be")


Find the sum of both elts of a tuple

>>> sumOf each tupleToSum
3

>>> foldrOf each (+) 0 tupleToSum
3

>>> getSum $ foldMapOf each Sum tupleToSum
3


------- 6.4 Higher order folds

>>> "Here's looking at you, kid" ^.. dropping 7 folded
"looking at you, kid"

>>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . folding words
["My","Precious","Hakuna","Matata","No","problemo"]

>>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (folding words)
["My","Hakuna","No"]

>>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. taking 2 (folded . folded)
"My"

>>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (folding words) . folded
"MyHakunaNo"

>>> sumOf (taking 2 each) (10, 50, 100)
60

>>> ("stressed", "guns", "evil") ^.. backwards each
["evil","guns","stressed"]

>>> ("stressed", "guns", "evil") ^.. backwards each . to reverse
["live","snug","desserts"]

>>> "blink182 k9 blazeit420" ^.. to words . folded . droppingWhile isAlpha folded
"1829420"

2. Solve the following using higher-order folds

how many days till the first thaw
>>> lengthOf (takingWhile (<0) folded) sample
2

warmest in first 4 days
>>> maximumOf (taking 4 folded) sample
Just 4

the temp on the day after hitting that temp. use preview or ^?
>>> let maxt = 4 in sample ^? dropping 1 (droppingWhile (/= maxt) folded) 
Just 3

How many days of below-freezing weather did we have consecutively at the END of the sample?
>>> lengthOf (takingWhile (<0) (backwards folded)) sample
2

Temp samples from the first time we sample above 0, until the next time we're below 0.

sample ^.. (takingWhile (>=0) . droppingWhile (<=0) $ folded)

-- the dot here is fn composition
>>> sample ^.. (droppingWhile (<0) . backwards . droppingWhile (<0) . backwards $ folded)
[4,3,8,6,-2,3]

>>> sample ^.. trimmingWhile (<0) folded
[4,3,8,6,-2,3]

-}

comparingOf :: Ord a => Lens' s a -> s -> s -> Ordering
comparingOf l = comparing (view l)


pairs :: [(Integer, String)]
pairs = [(2, "I'll"), (3, "Be"), (1, "Back")]

tupleToSum :: (Integer, Integer)
tupleToSum = (1, 2)

--------


sample :: [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

trimmingWhile p = droppingWhile p . backwards . droppingWhile p . backwards
