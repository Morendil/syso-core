module Main where

import Prelude

import Control.Monad.Except
import Data.Array hiding (mapWithIndex,insert)
import Data.Either
import Data.Foldable
import Data.Foreign
import Data.FunctorWithIndex
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.List.Types
import Data.Maybe
import Data.StrMap hiding (filter)
import Data.YAML.Foreign.Decode

type VariableName = String
type NameSpace = VariableName

type Targets = Array VariableName
type Missing = Array VariableName
type Rules = Array Rule
type Analysis = StrMap Evaluated
type Situation = StrMap Value

data Value = Numeric Number | Logic Boolean | Textual String | Uncomputed
derive instance eqValue :: Eq Value
derive instance gValue :: Generic Value _
instance showValue :: Show Value where
    show x = genericShow x

data Evaluated = Evaluated Value | Unknown Missing

data Rule = Rule NameSpace VariableName Formula
derive instance eqRule :: Eq Rule
derive instance gRule :: Generic Rule _
instance showRule :: Show Rule where
    show x = genericShow x

data Formula =
    Constant Number |
    VariableReference VariableName |
    Sum (Array Formula) |
    Mult { assiette :: Formula, taux :: Formula, plafond :: Formula }
derive instance eqFormula :: Eq Formula
derive instance gForm :: Generic Formula _
instance showForm :: Show Formula where
    show x = genericShow x

mult assiette taux plafond =
  (min assiette plafond) * taux

findRule :: Rules -> String -> Maybe Rule
findRule rules query =
    let isNamed q = (\ (Rule ns name _) -> (ns == "" && name == q) || (ns <> " . " <> name) ==  q)
    in head $ filter (isNamed query) rules

evaluate :: Rules -> Analysis -> VariableName -> Evaluated
evaluate rules analysis name =
    let evalNumeric formula = case evalFormula formula of
            Numeric num -> num
            _ -> 0.0
        evalFormula formula = case formula of
            Constant num -> Numeric num
            Sum values -> Numeric (sum $ map evalNumeric values)
            _ -> Uncomputed
    in case findRule rules name of
        (Just (Rule _ _ f)) -> Evaluated $ evalFormula f
        _ -> Evaluated Uncomputed

analyse :: Rules -> Targets -> Situation -> Analysis
analyse rules targets situation =
    let preanalysis = mapWithIndex (\ name value -> Evaluated value) situation
        store target storage = insert target (evaluate rules preanalysis target) storage
    in foldr store empty targets

valueOf :: Analysis -> String -> Value
valueOf analysis name = case lookup name analysis of
    (Just (Evaluated value)) -> value
    _ -> Uncomputed

missingVariables :: Analysis -> String -> Array String
missingVariables _ _ = []

parseRules :: String -> Either (NonEmptyList ForeignError) Rules
parseRules text = map makeRules (runExcept $ parseYAMLToJson text)

makeRules _ = []
