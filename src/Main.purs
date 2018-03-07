module Main where

import Prelude

import Control.Monad.Except
import Data.Array hiding (mapWithIndex,insert)
import Data.Either
import Data.Foldable
import Data.Traversable
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
type Analysis = StrMap (Maybe Value)
type Situation = StrMap Value

type Value = Number
type Operator = String

data Rule = Rule NameSpace VariableName Formula
derive instance eqRule :: Eq Rule
derive instance gRule :: Generic Rule _
instance showRule :: Show Rule where
    show x = genericShow x

data Formula =
    Constant Number |
    VariableReference VariableName |
    Sum (Array Formula) |
    Mult { assiette :: Formula, taux :: Formula, plafond :: Formula } |
    IfCondition BooleanFormula Formula |
    UnlessCondition BooleanFormula Formula
derive instance eqFormula :: Eq Formula
derive instance gForm :: Generic Formula _
instance showForm :: Show Formula where
    show x = genericShow x

data BooleanFormula =
	NumericComparison Operator Formula Formula |
	OneOfThese (Array BooleanFormula) |
	AnyOfThese (Array BooleanFormula) |
	EnumSelected VariableName
derive instance eqBooleanFormula :: Eq BooleanFormula
derive instance gBooleanFormula :: Generic BooleanFormula _
instance showBooleanFormula :: Show BooleanFormula where
    show x = genericShow x

mult assiette taux plafond =
  (min assiette plafond) * taux

findRule :: Rules -> String -> Maybe Rule
findRule rules query =
    let isNamed q = (\ (Rule ns name _) -> (ns == "" && name == q) || (ns <> " . " <> name) ==  q)
    in head $ filter (isNamed query) rules

evaluate :: Rules -> Analysis -> VariableName -> Maybe Value
evaluate rules analysis name =
    let evalFormula formula = case formula of
            Constant num -> Just num
            Sum values -> map sum $ sequence $ map evalFormula values
            _ -> Nothing
    in case findRule rules name of
        (Just (Rule _ _ f)) -> evalFormula f
        _ -> Nothing

analyse :: Rules -> Targets -> Situation -> Analysis
analyse rules targets situation =
    let preanalysis = map (\ value -> Just value) situation
        store target storage = insert target (evaluate rules preanalysis target) storage
    in foldr store empty targets

valueOf :: Analysis -> String -> Maybe Value
valueOf analysis name = join $ lookup name analysis

missingVariables :: Analysis -> String -> Array String
missingVariables _ _ = []

parseRules :: String -> Either (NonEmptyList ForeignError) Rules
parseRules text = map makeRules (runExcept $ parseYAMLToJson text)

makeRules _ = []
