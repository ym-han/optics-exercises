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


2. Exercises - Traversal Actions

a. Fill in the blanks

>>> sequenceAOf _1 (Nothing, "Rosebud")
Nothing




-}
