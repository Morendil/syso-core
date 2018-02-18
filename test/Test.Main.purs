module Test.Main where

import Main

import Prelude
import Data.Either

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Test.Unit.Assert

import Test.QuickCheck ((===))

main = runTest do
  suite "Evaluation" do
    test "Constants eval to themselves" do
      quickCheck $ \ num -> eval (Constant num) === num
  parsing

rules = """
- nom: nombre
  formule: 1
"""

parsing = suite "Parsing YAML representations" do
  test "Parse valid representation" do
    assert "Parse failed" $ isRight (parseRules rules)
  test "Parse invalid representation" do
    assert "Parse should fail" $ isLeft (parseRules ": a :")
