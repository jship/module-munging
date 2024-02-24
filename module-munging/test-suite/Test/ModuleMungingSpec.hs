{-# LANGUAGE BlockArguments #-}
module Test.ModuleMungingSpec
  ( spec
  ) where

import Data.String.Interpolate ()
import ModuleMunging qualified ()
import Prelude ()
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel do
  describe "Example" do
    it "works" do
      'a' `shouldBe` 'a'
