module Main where

data Constant = Constant Number

eval :: Constant -> Number
eval (Constant num) = num
