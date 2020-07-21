module Lux.Pair
    ( Pair (..)
    , lazy
    , strict
    ) where


data Pair a b = Pair !a !b

lazy :: Pair a b -> (a, b)
lazy (Pair x y) = (x, y)

strict :: (a, b) -> Pair a b
strict (x, y) = Pair x y
