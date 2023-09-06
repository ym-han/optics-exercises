{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chp7traversals where

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

import Control.Monad.State


-- ([(1, 2), (3, 4)], [5,6,7]) ^.. beside (folded . both) folded
{-
1. Exercises simple traversals

>>> ("Jurassic", "Park") & both .~ "N/A"
("N/A","N/A")

>>> ("Jurassic", "Park") & both . traversed .~ 'x'
("xxxxxxxx","xxxx")

>>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & beside id traversed %~ take 3
("Mal",["Kay","Ina","Jay"])

>>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . taking 1 (dropping 1 traversed) .~ "River"
("Malcolm",["Kaylee","River","Jayne"])

>>> ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . element 1 .~ "River"
("Malcolm",["Kaylee","River","Jayne"])

>>> ["Die Another Day", "Live and Let Die", "You Only Live Twice"] & traversed . elementOf worded 1 . traversed .~ 'x'
["Die xxxxxxx Day","Live xxx Let Die","You xxxx Live Twice"]

>>> ((1, 2), (3, 4)) & both . both +~ 1
((2,3),(4,5))

>>> (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1
(2,(3,[4,5]))

>>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each . traversed . filtered (/= "Blueberries") . dropping 7 (backwards traversed) %~ toUpper
((True,"STRAWberries"),(False,"Blueberries"),(True,"BLACKberries"))

>>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) & each %~ snd
("Strawberries","Blueberries","Blackberries")

7.4
2. Exercises - Traversal Actions

a. Fill in the blanks

>>> sequenceAOf _1 (Nothing, "Rosebud")
Nothing

-- tough:
sequenceAOf (traversed . _1) _ 



>>> sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]]
ZipList {getZipList = [[1,3],[2,4]]}

>>> sequenceAOf (traversed . _2) [('a', ZipList [1, 2]), ('b', ZipList [3, 4])]
ZipList {getZipList = [[('a',1),('b',3)],[('a',2),('b',4)]]}

>>> :t [('a', ZipList [1, 2]), ('b', ZipList [3, 4])]
[('a', ZipList [1, 2]), ('b', ZipList [3, 4])] :: Num a => [(Char, ZipList a)]

Note to self: The intuition i'm getting from CP's text re sequenceAOf is this. 
    sequenceAOf :: Traversal s t (f a) a -> s -> f t

    The thing to pay attention to is the type of what's focused by the traversal -- i.e., the `f a`.
    What sequenceAOf does, as CP says, is to pull that `f` to the outside of the resulting structure we get back.


State and traverseOf: useful!
>>> let result = traverseOf (beside traversed both) (\n -> modify (+n) >> get) ([1, 1, 1], (1, 1))
>>> runState result 0
(([1,2,3],(4,5)),5)

>>> :t result
result :: (Num b, MonadState b f) => f ([b], (b, b))
>>> :t (\n -> modify (+n) >> get) 
(\n -> modify (+n) >> get) :: (Num b, MonadState b m) => b -> m b

2. Rewrite the following using the infix-operator for traverseOf

traverseOf
    (_1 . traversed)
    (\c -> [toLower c, toUpper c]) 
    ("ab", True)

>>> ("ab", True) & (_1 . traversed) %%~ (\c -> [toLower c, toUpper c]) 
[("ab",True),("aB",True),("Ab",True),("AB",True)]

traverseOf
    (traversed . _1)
    (\c -> [toLower c, toUpper c])
    [('a', True), ('b', False)]

>>> [('a', True), ('b', False)] & (traversed . _1) %%~ (\c -> [toLower c, toUpper c])
[[('a',True),('b',False)],[('a',True),('B',False)],[('A',True),('b',False)],[('A',True),('B',False)]]

7.7 Exercises - partsOf

-- Viewing
>>> [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
[2,4]


>>> ["Aardvark", "Bandicoot", "Capybara"] ^. traversed . partsOf (taking 3 traversed)
"AarBanCap"

>>> ["Aardvark", "Bandicoot", "Capybara"] ^.. traversed . (taking 3 traversed)
"AarBanCap"

>>> ["Aardvark", "Bandicoot", "Capybara"] ^.. traversed . traversed
"AardvarkBandicootCapybara"

>>> ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf (beside traversed traversed)
[1,2,3,4]

-- Setting
>>> [1, 2, 3, 4] & partsOf (traversed . filtered even) .~ [20, 40]
[1,20,3,40]

>>> ["Aardvark", "Bandicoot", "Capybara"] & partsOf (taking 1 traversed . traversed) .~ "Kangaroo"
["Kangaroo","Bandicoot","Capybara"]

***
>>> ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Ant"
["Antdvark","Bandicoot","Capybara"]

-- Modifying
-- Tip: Map values are traversed in order by KEY

-- hack
>>> M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] & partsOf traversed %~ \s -> "bca"
fromList [('a','b'),('b','c'),('c','a')]

>>> ('a', 'b', 'c') & partsOf each %~ reverse
('c','b','a')

-}
