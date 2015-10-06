module Data.Extra.Num where

ensurePos :: (Eq a, Num a) => a -> a
ensurePos e = if signum e == (-1) then negate e else e

ensureNeg :: (Eq a, Num a) => a -> a
ensureNeg e = if signum e == 1 then negate e else e

class Similar a where
  sigma :: a -- margin of error

instance Similar Float where
  sigma = 0.01

instance Similar Double where
  sigma = 0.01

(=~) :: (Num a, Ord a, Similar a) => a -> a -> Bool
x =~ y = abs (x - y) < sigma

