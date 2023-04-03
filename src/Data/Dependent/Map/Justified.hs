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
import Data.Roles
import Data.Type.Coercion
import Prelude hiding (lookup, zip, zipWith)

import qualified Data.Dependent.Map as M

newtype DMap ph k v = DMap (M.DMap k v) deriving Typeable
type role DMap nominal nominal representational

newtype Key ph k i = Key (k i) deriving (GEq, GCompare)
type role Key nominal representational nominal

newtype Index ph i = Index Int deriving (Eq, Ord, Show)
type role Index nominal nominal

theDMap :: DMap ph k v -> M.DMap k v
theDMap (DMap m) = m

theKey :: Key ph k i -> k i
theKey (Key k) = k

theIndex :: Index ph i -> Int
theIndex (Index n) = n

withDMap :: M.DMap k v -- ^ The dependent map to use as input
         -> (forall ph. DMap ph k v -> t) -- ^ The computation to apply
         -> t -- ^ The resulting value
withDMap m cont = cont (DMap m)

withSingleton :: k i -> v i -> (forall ph. (Key ph k i, DMap ph k v) -> t) -> t
withSingleton k v cont = cont (Key k, DMap $ M.singleton k v)

data KeyInfo = Present | Missing deriving (Show, Eq, Ord)

type MissingReference k f i = (k i, f (k i, KeyInfo))
