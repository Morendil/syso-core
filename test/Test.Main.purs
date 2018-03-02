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
  parsing
  analysis

allRules = """
- nom: nombre
  formule: 1
"""

analysis = suite "Analysis" do

  test "Evaluate Constant" do
    let rules = [Rule "" "foo" (Constant 1.0)]
        analyzed = analyse rules ["foo"] empty
    equal (Numeric 1.0) (valueOf analyzed "foo")

  test "Evaluate Uncomputed" do
    let rules = [Rule "" "bar" (Constant 1.0)]
        analyzed = analyse rules ["foo"] empty
    equal (Uncomputed) (valueOf analyzed "foo")

parsing = suite "Parsing YAML representations" do

  test "Parse valid representation" do
    assert "Parse failed" $ isRight (parseRules allRules)

  test "Parse invalid representation" do
    assert "Parse should fail" $ isLeft (parseRules ": a :")
