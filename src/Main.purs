module Main where

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
import Data.Traversable
import Data.YAML.Foreign.Decode
import Prelude

import Data.Array (concatMap)

data Rule = Rule NameSpace VariableName Formula

data Formula =
    Constant Number |
    VariableReference VariableName |
    Sum (Array Formula)
    -- | Mult { assiette :: Formula, taux :: Formula, plafond :: Formula } |
    -- IfCondition BooleanFormula Formula |
    -- UnlessCondition BooleanFormula Formula

type VariableName = String
type NameSpace = VariableName

type Targets = Array VariableName
type Missing = Array VariableName
type Rules = Array Rule
type Analysis = StrMap (Either Value (Array VariableName))
type Situation = StrMap Value

type Value = Number
type Operator = String

derive instance eqRule :: Eq Rule
derive instance gRule :: Generic Rule _
instance showRule :: Show Rule where
    show x = genericShow x

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
            VariableReference var -> case lookup var analysis of
                Just (Left value) -> Just value
                _ -> Nothing
            Sum components -> map sum $ sequence $ map evalFormula components
            _ -> Nothing
    in case findRule rules name of
        (Just (Rule _ _ f)) -> evalFormula f
        _ -> Nothing

computeMissing :: Rules -> Analysis -> VariableName -> Array VariableName
computeMissing rules analysis name =
    let missingInFormula formula = case formula of
            Constant num -> []
            VariableReference var -> case lookup var analysis of
                Just (Left value) -> []
                _ -> [var]
            Sum components -> concatMap missingInFormula components
            _ -> []
    in case findRule rules name of
        (Just (Rule _ _ f)) -> missingInFormula f
        _ -> [name]

analyse :: Rules -> Targets -> Situation -> Analysis
analyse rules targets situation =
    let preanalysis = map (\ value -> Left value) situation
        maybeValue target = evaluate rules preanalysis target
        missingVars target = computeMissing rules preanalysis target
        evalOrMissing target = case maybeValue target of
            Nothing -> Right (missingVars target)
            Just value -> Left value
        store target storage = insert target (evalOrMissing target) storage
    in foldr store empty targets

valueOf :: Analysis -> String -> Maybe Value
valueOf analysis name = case lookup name analysis of
    Just (Left value) -> Just value
    Just (Right _) -> Nothing
    Nothing -> Nothing

missingVariables :: Analysis -> String -> Array String
missingVariables analysis name = case lookup name analysis of
    Just (Left value) -> []
    Just (Right missing) -> missing
    Nothing -> []

parseRules :: String -> Either (NonEmptyList ForeignError) Rules
parseRules text = map makeRules (runExcept $ parseYAMLToJson text)

makeRules _ = []
