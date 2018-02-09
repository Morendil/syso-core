module Test.Main where

import Main (Constant(..), eval)

import Prelude

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck ((===))

main = runTest do
  suite "Evaluation" do
    test "Constants eval to themselves" do
      quickCheck $ \ num -> eval (Constant num) === num
