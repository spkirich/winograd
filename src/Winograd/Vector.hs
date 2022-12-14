{-# LANGUAGE GADTs, StandaloneDeriving, TemplateHaskell #-}

-- | This module defines a non-empty type-safe vector.

module Winograd.Vector

  ( -- * Definition

    Vector (..)

  , S
  , Z

    -- * Construction

  , literal

    -- * Algebra

  , negate

  , add
  , dot

    -- * Other functions

  , zipWith

  ) where

import Prelude hiding (negate, zipWith)
import qualified Prelude

import Data.Traversable

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Z
data S n

-- | A vector.
data Vector n a where

  -- | A singleton vector.
  Singleton :: a -> Vector Z a

  -- | A vector with an element prepended.
  Extension :: a -> Vector n a -> Vector (S n) a

deriving instance Eq   a => Eq   (Vector n a)
deriving instance Show a => Show (Vector n a)

instance Functor (Vector n) where
  fmap = fmapDefault

instance Foldable (Vector n) where
  foldMap = foldMapDefault

instance Traversable (Vector n) where
  traverse f (Singleton x  ) = Singleton <$> f x
  traverse f (Extension x v) = Extension <$> f x <*> traverse f v

-- | A vector literal.
literal :: Lift a => [a] -> Q Exp
literal [       ] = error "the literal is empty"
literal [ x     ] = [| Singleton x              |]
literal ( x : v ) = [| Extension x $(literal v) |]

-- | Zip two vectors together with a function.
zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f (Singleton x  ) (Singleton y  ) = Singleton (f x y)
zipWith f (Extension x u) (Extension y v) = Extension (f x y) $ zipWith f u v

-- | Negate a vector.
negate :: Num a => Vector n a -> Vector n a
negate = fmap Prelude.negate

-- | Add two vectors.
add :: Num a => Vector n a -> Vector n a -> Vector n a
add = zipWith (+)

-- | Dot product of two vectors.
dot :: Num a => Vector n a -> Vector n a -> a
dot u = sum . zipWith (*) u
