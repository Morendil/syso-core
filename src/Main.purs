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
type Analysis = StrMap Evaluated
type Situation = StrMap Value
type Value = Number

data Evaluated = Evaluated (Maybe Value)

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
    let unwrap formula =
            let (Evaluated unwrapped) = evalFormula formula
            in unwrapped
        evalFormula formula = case formula of
            Constant num -> Evaluated (Just num)
            Sum values -> Evaluated (map sum $ sequence $ map unwrap values)
            _ -> Evaluated Nothing
    in case findRule rules name of
        (Just (Rule _ _ f)) -> evalFormula f
        _ -> Evaluated Nothing

analyse :: Rules -> Targets -> Situation -> Analysis
analyse rules targets situation =
    let preanalysis = map (\ value -> Evaluated (Just value)) situation
        store target storage = insert target (evaluate rules preanalysis target) storage
    in foldr store empty targets

valueOf :: Analysis -> String -> Maybe Value
valueOf analysis name = case lookup name analysis of
    Just (Evaluated value) -> value
    Nothing -> Nothing

missingVariables :: Analysis -> String -> Array String
missingVariables _ _ = []

parseRules :: String -> Either (NonEmptyList ForeignError) Rules
parseRules text = map makeRules (runExcept $ parseYAMLToJson text)

makeRules _ = []
