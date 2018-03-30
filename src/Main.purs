module Main where

import Prelude

import Control.Monad.State (State, runState, get, put)
import Data.Array (catMaybes, filter, head)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap (StrMap, insert, lookup)
import Data.TacitString (TacitString)
import Data.Traversable (class Traversable, foldr, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))
import Matryoshka (AlgebraM, cataM, embed)

data Rule = Rule NameSpace VariableName Formula
derive instance eqRule :: Eq Rule
derive instance gRule :: Generic Rule _
instance showRule :: Show Rule where
    show x = genericShow x

type Formula = Mu FormulaF
data FormulaF a =
    Input |
    Inversion (Array VariableName) |
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
  traverse f (Input) = pure (Input)
  traverse f (Inversion vars) = pure (Inversion vars)
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
    eq1 (Inversion v1) (Inversion v2) = eq v1 v2
    eq1 Input Input = true
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
inversionf vars = embed (Inversion vars)

mult assiette taux plafond =
  (min assiette plafond) * taux

findRule :: Rules -> String -> Maybe Rule
findRule rules query =
    let isNamed q = (\ (Rule ns name _) -> (ns == "" && name == q) || (ns <> " . " <> name) ==  q)
    in head $ filter (isNamed query) rules

foreign import uniroot :: (Number -> Number) -> Number -> Number -> Number -> Int -> Number

analyseAlgebra :: Rules -> VariableName -> AlgebraM (State Analysis) FormulaF Analyzed
analyseAlgebra rules name formula = case formula of
    Input -> pure (Left [name])
    Inversion vars -> do
        analysis <- get
        let getNameValue var = case lookup var analysis of
                Just value -> Just (Tuple var value)
                Nothing -> Nothing
            candidates = catMaybes $ map getNameValue vars
        case head candidates of
            Just (Tuple candidateName (Right candidateValue)) ->
                let errorForValue num = candidateValue - (extract $ tryValue num)
                    tryValue num = compute rules (insert name (Right num) analysis) candidateName
                    extract triedValue = case triedValue of
                        (Tuple (Right value) _) -> value
                        (Tuple (Left missing) _) -> candidateValue -- terminate; TODO this is incorrect
                in case tryValue 1000.0 of
                    (Tuple (Left missing) _) -> pure (Left missing)
                    (Tuple (Right value) _) -> pure (Right $ uniroot errorForValue 0.0 1000000.0 0.001 10)
            _ -> pure (Left vars)

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
    Sum components -> pure $ reduceWith (+) (<>) (Right 0.0) components

reduceWith :: (Value -> Value -> Value) -> (Missing -> Missing -> Missing) -> Analyzed -> Array Analyzed -> Analyzed
reduceWith combineValues concatMissing init analyzeds =
    let combine m1 @ (Left missing) (Right _) = m1
        combine (Right _) m2 @ (Left missing) = m2
        combine (Left m1) (Left m2) = Left $ concatMissing m1 m2
        combine (Right v1) (Right v2) = Right $ combineValues v1 v2
    in foldr combine init analyzeds

compute :: Rules -> Analysis -> VariableName -> Tuple Analyzed Analysis
compute rules analysis name =
    case findRule rules name of
        (Just (Rule _ _ formula)) ->
            let state = (cataM $ analyseAlgebra rules name) formula
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
