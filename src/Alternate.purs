module Alternate where

import Prelude

import Data.Foldable (sum)
import Data.Leibniz (type (~), coerceSymm)

type VariableName = String

data Formula t =
    --   Input
    --   t
    -- | VariableReference VariableName
    --   t
      Constant Number
      (t ~ Number)
    -- | Inversion (Array VariableName)
    --   (t ~ Number)      
    | Sum (Array (Formula Number))
      (t ~ Number)
    | ApplicableIf (Formula Boolean) (Formula Number)
      (t ~ Number)
    | Equality (Formula Number) (Formula Number)
      (t ~ Boolean)

eval :: forall a. Formula a -> a
eval (Constant x proof) = coerceSymm proof x
eval (Sum xs proof) = coerceSymm proof $ sum $ map eval xs
eval (Equality a b proof) = coerceSymm proof ((eval a) == (eval b))
eval (ApplicableIf cond value proof) = coerceSymm proof $ if eval cond then eval value else 0.0

foo :: Number
foo = eval expr
      where sums items = Sum items id
            applicableif cond value = ApplicableIf cond value id
            equality a b = Equality a b id
            constant num = Constant num id
            expr = sums [
                        applicableif (equality (constant 1.0) (constant 1.0)) (constant 0.5),
                        applicableif (equality (constant 1.0) (constant 0.0)) (constant 0.5),
                        applicableif (equality (constant 1.0) (constant 1.0)) (constant 0.5)
                    ]
