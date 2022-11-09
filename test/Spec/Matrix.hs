{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Spec.Matrix where

import Prelude hiding (negate)

import Test.Hspec
import Test.Hspec.QuickCheck

import Winograd
import Winograd.Matrix

import Spec.Common

e = $(literal
    [ [0, 0, 0 :: Integer]
    , [0, 0, 0 :: Integer]
    , [0, 0, 0 :: Integer]
    ]
  )

i = $(literal
    [ [1, 0, 0 :: Integer]
    , [0, 1, 0 :: Integer]
    , [0, 0, 1 :: Integer]
    ]
  )

spec :: Spec
spec = do

  describe "transpose of a matrix" $ do
    
    prop "is involutive" $ \(a :: Matrix D D Integer)
      -> (transpose . transpose) a == a

    prop "respects addition" $ \

        (a :: Matrix D D Integer)
        (b :: Matrix D D Integer)

      -> transpose (a `add` b) == transpose a `add` transpose b

  describe "addition of matrices" $ do

    prop "is commutative" $ \

        (a :: Matrix D D Integer)
        (b :: Matrix D D Integer)

      -> a `add` b == b `add` a

    prop "is associative" $ \

        (a :: Matrix D D Integer)
        (b :: Matrix D D Integer)
        (c :: Matrix D D Integer)

      -> a `add` (b `add` c) == (a `add` b) `add` c

    prop "has a neutral element" $ \(a :: Matrix D D Integer)
      -> a `add` e == a && e `add` a == a

    prop "is invertable" $ \(a :: Matrix D D Integer)
      -> a `add` negate a == e && negate a `add` a == e

  describe "multiplication of matrices" $ do

    prop "is associative" $ \

        (a :: Matrix D D Integer)
        (b :: Matrix D D Integer)
        (c :: Matrix D D Integer)

      -> a `multiply` (b `multiply` c) == (a `multiply` b) `multiply` c

    prop "has a neutral element" $ \(a :: Matrix D D Integer)
      -> a `multiply` i == a && i `multiply` a == a

    prop "is left distributive" $ \

        (a :: Matrix D D Integer)
        (b :: Matrix D D Integer)
        (c :: Matrix D D Integer)

      -> a `multiply` (b `add` c) == (a `multiply` b) `add` (a `multiply` c)

    prop "is right distributive" $ \

        (a :: Matrix D D Integer)
        (b :: Matrix D D Integer)
        (c :: Matrix D D Integer)

      -> (a `add` b) `multiply` c == (a `multiply` c) `add` (b `multiply` c)
