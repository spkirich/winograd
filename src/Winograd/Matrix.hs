{-# LANGUAGE TemplateHaskell #-}

-- | This module defines a non-empty type-safe matrix.

module Winograd.Matrix

  ( -- * Definition

    Matrix

    -- * Construction

  , literal

    -- * Algebra

  , add

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
