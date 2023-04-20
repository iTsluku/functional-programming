module App where

import MyMath

addThree :: Integer -> Integer -> Integer -> Integer
addThree a b c = add (add a b) c
