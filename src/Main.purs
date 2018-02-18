module Main where

import Prelude

import Data.Either
import Data.List.Types
import Data.Foreign
import Data.StrMap
import Data.YAML.Foreign.Decode
import Control.Monad.Except

data Rules = Rules

data Constant = Constant Number

eval :: Constant -> Number
eval (Constant num) = num

valueOf :: String -> StrMap Number -> Rules -> Number
valueOf _ _ _ = 0.0

parseRules :: String -> Either (NonEmptyList ForeignError) Rules
parseRules text = map makeRules (runExcept $ parseYAMLToJson text)

makeRules _ = Rules
