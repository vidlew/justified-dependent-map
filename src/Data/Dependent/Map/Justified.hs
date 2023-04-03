{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Dependent.Map.Justified (
    DMap
  , Key
  , Index
  ) where

import Data.GADT.Compare
import Data.Typeable
import Prelude hiding (lookup, zip, zipWith)

import qualified Data.Dependent.Map as M

newtype DMap ph k v = DMap (M.DMap k v) deriving Typeable
type role DMap nominal nominal representational

newtype Key ph k i = Key (k i) deriving (GEq, GCompare)
type role Key nominal representational nominal

newtype Index ph = Index Int deriving (Eq, Ord, Show)
type role Index nominal
