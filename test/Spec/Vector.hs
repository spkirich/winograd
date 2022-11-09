{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Spec.Vector where

import Prelude hiding (negate)

import Test.Hspec
import Test.Hspec.QuickCheck

import Winograd
import Winograd.Vector

import Spec.Common

e = $(literal [0, 0, 0 :: Integer])

spec :: Spec
spec = do

  describe "addition of vectors" $ do

    prop "is commutative" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)

      -> u `add` v == v `add` u

    prop "is associative" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)
        (w :: Vector D Integer)

      -> u `add` (v `add` w) == (u `add` v) `add` w

    prop "has a neutral element" $ \(v :: Vector D Integer)
      -> v `add` e == v && e `add` v == v

    prop "is invertable" $ \(v :: Vector D Integer)
      -> v `add` negate v == e && negate v `add` v == e

  describe "dot product of vectors" $ do

    prop "is commutative" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)

      -> u `dot` v == v `dot` u

    prop "is left distributive" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)
        (w :: Vector D Integer)

      -> u `dot` (v `add` w) == (u `dot` v) + (u `dot` w)

    prop "is right distributive" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)
        (w :: Vector D Integer)

      -> (u `add` v) `dot` w == (u `dot` w) + (v `dot` w)
