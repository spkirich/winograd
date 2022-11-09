import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

import Winograd

import Winograd.Matrix (Matrix)
import qualified Winograd.Matrix as Matrix

import qualified Spec.Vector as Vector
import qualified Spec.Matrix as Matrix

spec :: Spec
spec = do

  Vector.spec
  Matrix.spec

main :: IO ()
main = hspec spec
