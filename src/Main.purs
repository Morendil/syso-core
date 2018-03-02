module Main where

import Prelude

import Data.Either
import Data.Foldable
import Data.FunctorWithIndex
import Data.List.Types
import Data.Foreign
import Data.StrMap
import Data.YAML.Foreign.Decode
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Control.Monad.Except

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

data Formula =
    Constant Number |
    VariableReference VariableName |
    Mult { assiette :: Formula, taux :: Formula, plafond :: Formula }

mult assiette taux plafond =
  (min assiette plafond) * taux

eval :: Rules -> Analysis -> VariableName -> Evaluated
eval _ _ _ = Evaluated Uncomputed

analyse :: Rules -> Targets -> Situation -> Analysis
analyse rules targets situation =
    let preanalysis = mapWithIndex (\ name value -> Evaluated value) situation
        store target storage = insert target (eval rules preanalysis target) storage
    in foldr store empty targets

valueOf :: Analysis -> String -> Value
valueOf _ _ = Uncomputed

missingVariables :: Analysis -> String -> Array String
missingVariables _ _ = []

parseRules :: String -> Either (NonEmptyList ForeignError) Rules
parseRules text = map makeRules (runExcept $ parseYAMLToJson text)

makeRules _ = []
