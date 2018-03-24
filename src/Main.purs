module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.State (State, runState, get, put)
import Data.Array (concat, filter, head)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Foreign (ForeignError)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap (StrMap, empty, insert, lookup)
import Data.TacitString (TacitString)
import Data.Tuple (Tuple(..))
import Data.Traversable (class Traversable, sequenceDefault, foldr, traverse, sequence, sum)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Matryoshka (Algebra, AlgebraM, cata, cataM, embed)

data Rule = Rule NameSpace VariableName Formula
derive instance eqRule :: Eq Rule
derive instance gRule :: Generic Rule _
instance showRule :: Show Rule where
    show x = genericShow x

type Formula = Mu FormulaF
data FormulaF a =
    Constant Number |
    VariableReference VariableName |
    Sum (Array a)
    -- | Mult { assiette :: Formula, taux :: Formula, plafond :: Formula } |
    -- IfCondition BooleanFormula Formula |
    -- UnlessCondition BooleanFormula Formula

derive instance functorFomulaF :: Functor FormulaF
derive instance gFormula :: Generic (FormulaF TacitString) _
instance showFormula :: Show (FormulaF TacitString) where
    show x = genericShow x
instance traversableFormulaF :: Traversable FormulaF where
  -- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  traverse f (Sum children) = Sum <$> traverse f children
  traverse f (Constant v) = pure (Constant v)
  traverse f (VariableReference v) = pure (VariableReference v)
  sequence f = sequenceDefault f
instance foldableFormulaF :: Foldable FormulaF where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  foldMap f (Sum children) = foldMap f children
  foldMap f _ = mempty

instance eq1FormulaF :: Eq1 FormulaF where
    eq1 (Constant n1) (Constant n2) = eq n1 n2
    eq1 (VariableReference n1) (VariableReference n2) = eq n1 n2
    eq1 (Sum n1) (Sum n2) = eq n1 n2
    eq1 _ _ = false

type VariableName = String
type NameSpace = VariableName

type Targets = Array VariableName
type Missing = Array VariableName
type Rules = Array Rule
type Analyzed = Either Missing Value
type Analysis = StrMap Analyzed
type Situation = StrMap Value

type Value = Number
type Operator = String

{-data BooleanFormula =
    NumericComparison Operator Formula Formula |
    OneOfThese (Array BooleanFormula) |
    AnyOfThese (Array BooleanFormula) |
    EnumSelected VariableName
-}

-- Fixed point constructors
sumf items = embed (Sum items)
constantf value = embed (Constant value)
variableref var = embed (VariableReference var)

mult assiette taux plafond =
  (min assiette plafond) * taux

findRule :: Rules -> String -> Maybe Rule
findRule rules query =
    let isNamed q = (\ (Rule ns name _) -> (ns == "" && name == q) || (ns <> " . " <> name) ==  q)
    in head $ filter (isNamed query) rules

analyseAlgebra :: Rules -> AlgebraM (State Analysis) FormulaF Analyzed
analyseAlgebra rules formula = case formula of
    Constant num -> pure (Right num)
    VariableReference var -> do
            analysis <- get
            let cached = lookup var analysis
            case cached of
                Just analysed -> pure analysed
                Nothing -> do
                    let (Tuple result updated) = compute rules analysis var
                    put (insert var result updated)
                    pure result
    Sum components -> pure $ sum components

compute :: Rules -> Analysis -> VariableName -> Tuple Analyzed Analysis
compute rules analysis name =
    case findRule rules name of
        (Just (Rule _ _ formula)) ->
            let state = (cataM $ analyseAlgebra rules) formula
                (Tuple result updated) = runState state analysis
            in Tuple result (insert name result updated)
        _ ->
            let error = Left [name]
            in Tuple error (insert name error analysis)

analyse :: Rules -> Targets -> Situation -> Analysis
analyse rules targets situation =
    let preanalysis = map (\ value -> Right value) situation
        analyseOne name analysis =
            let (Tuple result analysis) = compute rules analysis name
            in analysis
    in foldr analyseOne preanalysis targets

valueOf :: Analysis -> String -> Maybe Value
valueOf analysis name = case lookup name analysis of
    Just (Right value) -> Just value
    Just (Left _) -> Nothing
    Nothing -> Nothing

missingVariables :: Analysis -> String -> Array String
missingVariables analysis name = case lookup name analysis of
    Just (Right value) -> []
    Just (Left missing) -> missing
    Nothing -> []

parseRules :: String -> Either (NonEmptyList ForeignError) Rules
parseRules text = map makeRules (runExcept $ parseYAMLToJson text)

makeRules _ = []
