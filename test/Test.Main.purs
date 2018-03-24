module Test.Main where

import Data.Either (isLeft, isRight)
import Data.Maybe
import Main
import Prelude
import Test.Unit.Assert

import Data.StrMap (StrMap, insert, empty)
import Test.QuickCheck ((===))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

main = runTest do
  parsing
  rulebase
  analysis
  missing

analysis = suite "Analysis" do

  test "Evaluate Constant" do
    let rules = [Rule "" "foo" (constantf 1.0)]
        analyzed = analyse rules ["foo"] empty
    equal (Just 1.0) (valueOf analyzed "foo")

  test "Evaluate Sum" do
    let rules = [Rule "" "foo" (sumf [constantf 1.0, constantf 1.0])]
        analyzed = analyse rules ["foo"] empty
    equal (Just 2.0) (valueOf analyzed "foo")

  test "Evaluate Sum with missing variable" do
    let rules = [Rule "" "foo" (sumf [constantf 1.0, variableref "bar"])]
        analyzed = analyse rules ["foo"] empty
    equal Nothing (valueOf analyzed "foo")

  test "Evaluate Sum with variable supplied in situation" do
    let rules = [Rule "" "foo" (sumf [constantf 1.0, variableref "bar"])]
        analyzed = analyse rules ["foo"] (insert "bar" 2.0 empty)
    equal (Just 3.0) (valueOf analyzed "foo")

  test "Evaluate Sum with variable reference" do
    let rules = [Rule "" "foo" (sumf [constantf 1.0, variableref "bar"]),
                 Rule "" "bar" (constantf 1.5)]
        analyzed = analyse rules ["foo"] empty
    equal (Just 2.5) (valueOf analyzed "foo")

  test "Evaluate unknown variable" do
    let rules = [Rule "" "bar" (constantf 1.0)]
        analyzed = analyse rules ["foo"] empty
    equal Nothing (valueOf analyzed "foo")

missing = suite "Missing variables" do

  test "Constant never has missing variables" do
    let rules = [Rule "" "foo" (constantf 1.0)]
        analyzed = analyse rules ["foo"] empty
    equal [] (missingVariables analyzed "foo")

  test "VariableReference adds a missing if absent in situation" do
    let rules = [Rule "" "foo" (variableref "bar")]
        analyzed = analyse rules ["foo"] empty
    equal ["bar"] (missingVariables analyzed "foo")

  test "VariableReference doesn't add a missing if provided in situation" do
    let rules = [Rule "" "foo" (variableref "bar")]
        analyzed = analyse rules ["foo"] (insert "bar" 1.0 empty)
    equal [] (missingVariables analyzed "foo")

  test "Sum propagates missing variables absent in situation" do
    let rules = [Rule "" "foo" (sumf [constantf 1.0, variableref "bar"])]
        analyzed = analyse rules ["foo"] empty
    equal ["bar"] (missingVariables analyzed "foo")

  test "Sum doesn't propagate as missing variables provided in situation" do
    let rules = [Rule "" "foo" (sumf [variableref "bar", constantf 1.0])]
        analyzed = analyse rules ["foo"] (insert "bar" 1.0 empty)
    equal [] (missingVariables analyzed "foo")

allRules = """
- nom: nombre
  formule: 1
"""

rulebase = suite "Rule base" do

  test "Find a rule" do
    let rule1 = Rule "" "foo" (constantf 1.0)
        rules = [rule1]
    equal (Just rule1) (findRule rules "foo")

parsing = suite "Parsing YAML representations" do

  test "Parse valid representation" do
    assert "Parse failed" $ isRight (parseRules allRules)

  test "Parse invalid representation" do
    assert "Parse should fail" $ isLeft (parseRules ": a :")
