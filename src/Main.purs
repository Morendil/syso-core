module Main where

import Prelude

import Control.Monad.State (State, runState, get, put)
import Data.Array (catMaybes, filter, foldr, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, insert, lookup)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

type VariableName = String
type NameSpace = VariableName

type Targets = Array VariableName
type Missing = Array VariableName
type Rules = Array Rule
type Analyzed = Either Missing Value
type Analysis = StrMap Analyzed
type Situation = StrMap Value

type Value = Number
type Operator = (Number -> Number -> Boolean)

data Rule = Rule NameSpace VariableName Formula

data Formula =
      Input
    | Inversion (Array VariableName)
    | Constant Number
    | VariableReference VariableName
    | Sum (Array Formula)
    | IfCondition BooleanFormula Formula
    -- | Mult { assiette :: Formula, taux :: Formula, plafond :: Formula } |
    -- UnlessCondition BooleanFormula Formula

data BooleanFormula =
      NumericComparison Operator Formula Formula
    -- | OneOfThese (Array BooleanFormula)
    -- | AnyOfThese (Array BooleanFormula)
    -- | EnumSelected VariableName

findRule :: Rules -> String -> Maybe Rule
findRule rules query =
    let isNamed q = (\ (Rule ns name _) -> (ns == "" && name == q) || (ns <> " . " <> name) ==  q)
    in head $ filter (isNamed query) rules

foreign import uniroot :: (Number -> Number) -> Number -> Number -> Number -> Int -> Number

eval :: Rules -> VariableName -> Formula -> State Analysis Analyzed
eval rules name formula = case formula of
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
    Sum components -> do
        analysis <- get
        let state = sequence $ map (eval rules name) components
            (Tuple terms updated) = runState state analysis
        put updated
        pure $ reduceWith (+) (<>) (Right 0.0) terms

    IfCondition condF valueF -> do
        condA <- evalBoolean rules name condF
        value <- eval rules name valueF
        case condA of
            (Left missing) -> pure (Left missing)
            (Right cond) -> if cond then pure value else pure (Right 0.0)

evalBoolean :: Rules -> VariableName -> BooleanFormula -> State Analysis (Either Missing Boolean)
evalBoolean rules name formula = case formula of
    (NumericComparison op v1F v2F) -> do
        v1A <- eval rules name v1F
        v2A <- eval rules name v2F
        case (Tuple v1A v2A) of
            (Tuple (Right v1) (Right v2)) -> pure (Right $ op v1 v2)
            (Tuple (Right v1) (Left m2)) -> pure (Left m2)
            (Tuple (Left m1) (Right v2)) -> pure (Left m1)
            (Tuple (Left m1) (Left m2)) -> pure (Left $ m1 <> m2)

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
            let state = eval rules name formula
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
