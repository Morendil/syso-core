module Test.Main where

import Prelude

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Test.Unit.Assert as Assert

import Test.QuickCheck (Result(), (===))

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

main = runTest do
  suite "mix it up" do
    test "Let's succeed" do
      Assert.equal 4 (2 + 2)
    test "the commutative property" do
      quickCheck theCommutativeProperty
