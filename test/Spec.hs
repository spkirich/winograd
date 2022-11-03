{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

import Winograd.Vector (Vector)
import qualified Winograd.Vector as Vector

instance Arbitrary a => Arbitrary (Vector Vector.Z a) where
  arbitrary = Vector.Singleton <$> arbitrary

instance (Arbitrary a, Arbitrary (Vector n a)) => Arbitrary (Vector (Vector.S n) a) where
  arbitrary = Vector.Extension <$> arbitrary <*> arbitrary

type Dim = Vector.S (Vector.S Vector.Z)

spec :: Spec
spec = do

  describe "vector addition" $ do

    prop "is commutative" $ \

        (u :: Vector Dim Integer)
        (v :: Vector Dim Integer)

      -> let

          l = u `Vector.add` v
          r = v `Vector.add` u

        in l == r

    prop "is associative" $ \

        (u :: Vector Dim Integer)
        (v :: Vector Dim Integer)
        (w :: Vector Dim Integer)

      -> let

          l = u `Vector.add` (v `Vector.add` w)
          r = (u `Vector.add` v) `Vector.add` w

        in l == r

    prop "has a neutral element" $ \

        (v :: Vector Dim Integer)

      -> let

          e = $(Vector.literal [0, 0, 0 :: Integer])

          l = v `Vector.add` e
          r = e `Vector.add` v

        in l == v && r == v

    prop "has a negative for every element" $ \
    
        (v :: Vector Dim Integer)
  
      -> let

          n = negate <$> v

          l = v `Vector.add` n
          r = n `Vector.add` v

          e = $(Vector.literal [0, 0, 0 :: Integer])

        in l == e && r == e

  describe "vector multiplication" $ do

    prop "is commutative" $ \

        (u :: Vector Dim Integer)
        (v :: Vector Dim Integer)

      -> let

          l = u `Vector.multiply` v
          r = v `Vector.multiply` u

        in l == r

    prop "is left distributive" $ \

        (u :: Vector Dim Integer)
        (v :: Vector Dim Integer)
        (w :: Vector Dim Integer)

      -> let

          l = u `Vector.multiply` (v `Vector.add` w)
          r = (u `Vector.multiply` v) + (u `Vector.multiply` w) 

        in l == r

    prop "is right distributive" $ \

        (u :: Vector Dim Integer)
        (v :: Vector Dim Integer)
        (w :: Vector Dim Integer)

      -> let

          l = (u `Vector.add` v) `Vector.multiply` w
          r = (u `Vector.multiply` w) + (v `Vector.multiply` w) 

        in l == r

    prop "is non-negative as a square" $ \

        (v :: Vector Dim Integer)

      -> v `Vector.multiply` v >= 0

main :: IO ()
main = hspec spec