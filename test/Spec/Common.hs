{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Spec.Common where

import Test.QuickCheck
import Winograd.Vector

instance Arbitrary a => Arbitrary (Vector Z a) where
  arbitrary = Singleton <$> arbitrary

instance (Arbitrary a, Arbitrary (Vector n a)) => Arbitrary (Vector (S n) a) where
  arbitrary = Extension <$> arbitrary <*> arbitrary

type D = S (S Z)
