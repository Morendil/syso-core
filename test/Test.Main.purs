module Test.Main where

import Main

import Prelude
import Data.Either
import Data.StrMap (StrMap, empty)

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
  evaluation

rules = """
- nom: nombre
  formule: 1
"""

emptyDict = empty :: StrMap Number

evaluation = suite "Evaluation" do
  test "Evaluate with an empty map" do
    equal (valueOf "nombre" emptyDict holder) 1.0
    where
      holder = case parseRules rules of
        Right result -> result
        Left error -> Rules

parsing = suite "Parsing YAML representations" do
  test "Parse valid representation" do
    assert "Parse failed" $ isRight (parseRules rules)
  test "Parse invalid representation" do
    assert "Parse should fail" $ isLeft (parseRules ": a :")
