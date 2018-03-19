module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (concat, filter, head)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Foreign (ForeignError)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, empty, insert, lookup)
import Data.TacitString (TacitString)
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
type Analysis = StrMap (Either Value (Array VariableName))
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

evaluate :: Rules -> Analysis -> VariableName -> Maybe Value
evaluate rules analysis name =
    let evalFormula formula = case formula of
            Constant num -> Just num
            VariableReference var -> case lookup var analysis of
                Just (Left value) -> Just value
                _ -> evaluate rules analysis var
            Sum components -> map sum $ sequence components
    in case findRule rules name of
        (Just (Rule _ _ f)) -> (cata evalFormula) f
        _ -> Nothing

missingAlgebra :: Rules -> Analysis -> Algebra FormulaF Missing
missingAlgebra rules analysis formula = case formula of
    Constant num -> []
    VariableReference var -> case lookup var analysis of
        Just (Left value) -> []
        _ -> [var]
    Sum components -> concat components

missingAlgebraM :: Rules -> Analysis -> AlgebraM Identity FormulaF Missing
missingAlgebraM rules analysis formula = case formula of
    Constant num -> pure []
    VariableReference var -> case lookup var analysis of
        Just (Left value) -> pure []
        _ -> pure [var]
    Sum components -> pure $ concat components

computeMissing :: Rules -> Analysis -> VariableName -> Array VariableName
computeMissing rules analysis name =
    case findRule rules name of
        (Just (Rule _ _ f)) -> unwrap $ (cataM $ missingAlgebraM rules analysis) f
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
