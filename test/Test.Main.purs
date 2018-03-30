module Test.Main where

import Main
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Number.Approximate ((≅))
import Data.StrMap (empty, insert)
import Data.Tuple (Tuple(..))
import Test.Unit (TestF, Test, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Eff (_) Unit
main = runTest do
  analysis
  inversion
  missing

aboutEqual :: forall e. Maybe Number -> Maybe Number -> Test e
aboutEqual a b = case (Tuple a b) of
  Tuple (Just va) (Just vb) -> assert ("Approximate comparison failed between" <> (show va) <> "and" <> (show vb)) (va ≅ vb)
  Tuple _ _ -> assert "One of the compared numbers is Nothing" false

analysis :: forall a. Free (TestF a) Unit
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

inversion :: forall a. Free (TestF a) Unit
inversion = suite "Inversion" do
  test "Evaluate inverted Sum with variable supplied in situation" do
    let rules = [Rule "" "net" (sumf [variableref "brut", variableref "taxe"]),
                 Rule "" "brut" (inversionf ["net"]),
                 Rule "" "taxe" (constantf 1.6)
                ]
        analyzed = analyse rules ["brut"] (insert "net" 3.0 empty)
    aboutEqual (Just 1.4) (valueOf analyzed "brut")

missing :: forall a. Free (TestF a) Unit
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

  test "Sum concatenates missing variables" do
    let rules = [Rule "" "foo" (sumf [variableref "bar", variableref "qux"])]
        analyzed = analyse rules ["foo"] empty
    equal ["bar", "qux"] (missingVariables analyzed "foo")

  test "Sum doesn't propagate as missing variables provided in situation" do
    let rules = [Rule "" "foo" (sumf [variableref "bar", constantf 1.0])]
        analyzed = analyse rules ["foo"] (insert "bar" 1.0 empty)
    equal [] (missingVariables analyzed "foo")
