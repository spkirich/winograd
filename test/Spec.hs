{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

import Winograd

import Winograd.Vector (Vector)
import qualified Winograd.Vector as Vector

import Winograd.Matrix (Matrix)
import qualified Winograd.Matrix as Matrix

instance Arbitrary a => Arbitrary (Vector Z a) where
  arbitrary = Singleton <$> arbitrary

instance (Arbitrary a, Arbitrary (Vector n a)) => Arbitrary (Vector (S n) a) where
  arbitrary = Extension <$> arbitrary <*> arbitrary

type D = S (S Z)

spec :: Spec
spec = do

  describe "vector addition" $ do

    prop "is commutative" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)

      -> let

          l = u `Vector.add` v
          r = v `Vector.add` u

        in l == r

    prop "is associative" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)
        (w :: Vector D Integer)

      -> let

          l = u `Vector.add` (v `Vector.add` w)
          r = (u `Vector.add` v) `Vector.add` w

        in l == r

    prop "has a neutral element" $ \

        (v :: Vector D Integer)

      -> let

          e = $(Vector.literal [0, 0, 0 :: Integer])

          l = v `Vector.add` e
          r = e `Vector.add` v

        in l == v && r == v

    prop "has a negative for every element" $ \
    
        (v :: Vector D Integer)
  
      -> let

          n = negate <$> v

          l = v `Vector.add` n
          r = n `Vector.add` v

          e = $(Vector.literal [0, 0, 0 :: Integer])

        in l == e && r == e

  describe "vector multiplication" $ do

    prop "is commutative" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)

      -> let

          l = u `Vector.multiply` v
          r = v `Vector.multiply` u

        in l == r

    prop "is left distributive" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)
        (w :: Vector D Integer)

      -> let

          l = u `Vector.multiply` (v `Vector.add` w)
          r = (u `Vector.multiply` v) + (u `Vector.multiply` w) 

        in l == r

    prop "is right distributive" $ \

        (u :: Vector D Integer)
        (v :: Vector D Integer)
        (w :: Vector D Integer)

      -> let

          l = (u `Vector.add` v) `Vector.multiply` w
          r = (u `Vector.multiply` w) + (v `Vector.multiply` w) 

        in l == r

    prop "is non-negative as a square" $ \

        (v :: Vector D Integer)

      -> v `Vector.multiply` v >= 0

  describe "matrix addition" $ do

    prop "is commutative" $ \

        (u :: Matrix D D Integer)
        (v :: Matrix D D Integer)

      -> let

          l = u `Matrix.add` v
          r = v `Matrix.add` u

        in l == r

    prop "is associative" $ \

        (u :: Matrix D D Integer)
        (v :: Matrix D D Integer)
        (w :: Matrix D D Integer)

      -> let

          l = u `Matrix.add` (v `Matrix.add` w)
          r = (u `Matrix.add` v) `Matrix.add` w

        in l == r

    prop "has a neutral element" $ \

        (v :: Matrix D D Integer)

      -> let

          e = $(Matrix.literal
              [ [0, 0, 0 :: Integer]
              , [0, 0, 0 :: Integer]
              , [0, 0, 0 :: Integer]
              ]
            )

          l = v `Matrix.add` e
          r = e `Matrix.add` v

        in l == v && r == v

    prop "has a negative for every element" $ \
    
        (v :: Matrix D D Integer)
  
      -> let

          n = (negate <$>) <$> v

          l = v `Matrix.add` n
          r = n `Matrix.add` v

          e = $(Matrix.literal
              [ [0, 0, 0 :: Integer]
              , [0, 0, 0 :: Integer]
              , [0, 0, 0 :: Integer]
              ]
            )

        in l == e && r == e

main :: IO ()
main = hspec spec
