{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chp2 where


import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Ship = Ship
  { _name :: String,
    _numCrew :: Int
  }
  deriving (Show)

-- name :: Lens' Ship String
-- name = lens getName setName 
--     where 
--         getName = _name
--         setName ship newName = ship{_name = newName }    


--- auto generating field lenses with TH

makeLenses ''Ship