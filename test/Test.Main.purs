module Test.Main where

import Data.Either
import Data.Maybe
import Main
import Prelude
import Test.Unit.Assert

import Data.StrMap (StrMap, empty)
import Test.QuickCheck ((===))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

main = runTest do
  parsing
  rulebase
  analysis

allRules = """
- nom: nombre
  formule: 1
"""

rulebase = suite "Rule base" do

  test "Find a rule" do
    let rule1 = Rule "" "foo" (Constant 1.0)
        rules = [rule1]
    equal (Just rule1) (findRule rules "foo")

analysis = suite "Analysis" do

  test "Evaluate Constant" do
    let rules = [Rule "" "foo" (Constant 1.0)]
        analyzed = analyse rules ["foo"] empty
    equal (Just 1.0) (valueOf analyzed "foo")

  test "Evaluate Sum" do
    let rules = [Rule "" "foo" (Sum [Constant 1.0, Constant 1.0])]
        analyzed = analyse rules ["foo"] empty
    equal (Just 2.0) (valueOf analyzed "foo")

  test "Evaluate Sum with missing variable" do
    let rules = [Rule "" "foo" (Sum [Constant 1.0, VariableReference "bar"])]
        analyzed = analyse rules ["foo"] empty
    equal Nothing (valueOf analyzed "foo")

  test "Evaluate Uncomputed" do
    let rules = [Rule "" "bar" (Constant 1.0)]
        analyzed = analyse rules ["foo"] empty
    equal Nothing (valueOf analyzed "foo")

parsing = suite "Parsing YAML representations" do

  test "Parse valid representation" do
    assert "Parse failed" $ isRight (parseRules allRules)

  test "Parse invalid representation" do
    assert "Parse should fail" $ isLeft (parseRules ": a :")
