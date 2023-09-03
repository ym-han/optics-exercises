{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module FoldsFiltering where

import Control.Lens
import Data.List (isPrefixOf)

data Card =
    Card { _name    :: String
         , _aura    :: Aura
         , _holo    :: Bool -- Is the card holographic
         , _moves   :: [Move]
         } deriving (Show, Eq)

data Move =
    Move { _moveName  :: String
         , _movePower :: Int
         } deriving (Show, Eq)

-- Each card has an aura-type
data Aura
    = Wet
    | Hot
    | Spark
    | Leafy
    deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck :: [Card]
deck = [ Card "Skwortul"    Wet False   [Move "Squirt" 20]
       , Card "Scorchander" Hot False   [Move "Scorch" 20]
       , Card "Seedasaur"   Leafy False [Move "Allergize" 20]
       , Card "Kapichu"     Spark False [Move "Poke" 10 , Move "Zap" 30]
       , Card "Elecdude"    Spark False [Move "Asplode" 50]
       , Card "Garydose"    Wet True    [Move "Gary's move" 40]
       , Card "Moisteon"    Wet False   [Move "Soggy" 3]
       , Card "Grasseon"    Leafy False [Move "Leaf Cut" 30]
       , Card "Spicyeon"    Hot False   [Move "Capsaicisize" 40]
       , Card "Sparkeon"    Spark True  [Move "Shock" 40 , Move "Battery" 50]
       ]

-- 1. List all the cards whose name starts with 'S'

-- deck
--  ^.. folded 
--    . filtered (isPrefixOf "S" . _name)
-- TODO: Look into how to do this with just folds and filteredBy

{-
2. What's the lowest attack power of all moves?

>>> minimumOf (folded . moves . folded . movePower) deck
Just 3


3. What’s the name of the first card which has more than one move?

>>> deck ^? folded . filtered (\card -> (lengthOf moves card) > 0) . name
Just "Skwortul"


4. Are there any Hot cards with a move with more than 30 attack power?

5. List the names of all holographic cards with a Wet aura.

6. What’s the sum of all attack power for all moves belonging to non-Leafy cards?

-}
