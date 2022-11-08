{-# LANGUAGE GADTs, TemplateHaskell #-}

-- | This module defines a non-empty type-safe matrix.

module Winograd.Matrix

  ( -- * Definition

    Matrix

    -- * Construction

  , literal

    -- * Algebra

  , add
  , multiply

  -- * Other functions

  , transpose

  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Winograd.Vector (Vector)
import qualified Winograd.Vector as Vector

-- | A matrix.
type Matrix m n a = Vector m (Vector n a)

-- | A matrix literal.
literal :: Lift a => [[a]] -> Q Exp
literal [       ] = error "the literal is empty"
literal [ v     ] = [| Vector.Singleton $(Vector.literal v)              |]
literal ( v : a ) = [| Vector.Extension $(Vector.literal v) $(literal a) |]

-- | Add two matrices.
add :: Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
add = Vector.zipWith Vector.add

-- | Transpose a matrix.
transpose :: Matrix m n a -> Matrix n m a

transpose (Vector.Singleton (Vector.Singleton x  )) = Vector.Singleton (Vector.Singleton x)
transpose (Vector.Singleton (Vector.Extension x v)) = Vector.Extension (Vector.Singleton x) . transpose $ Vector.Singleton v

transpose (Vector.Extension v m) = Vector.zipWith Vector.Extension v $ transpose m

-- | Multiply two matrices with a textbook algorithm.
multiply :: Num a => Matrix m n a -> Matrix n k a -> Matrix m k a
multiply m n = fmap (\u -> (\v -> u `Vector.multiply` v) <$> transpose n) m
