{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chp4 where


import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

{-
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


-}
