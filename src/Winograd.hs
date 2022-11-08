-- | This module provides general types and functions.

module Winograd

  ( -- * Definition

    Matrix
  , Vector (..)

  , S
  , Z

    -- * Matrix multiplication

  , multiply

  ) where

import Winograd.Vector hiding (multiply)
import Winograd.Matrix
