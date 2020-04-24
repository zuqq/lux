module Lux.Color
    ( Color (..)
    , average
    , black
    , mix
    , sky
    , white
    ) where

import Lux.Vector (Vector (..), unit)


data Color = Color
    { cR :: {-# UNPACK #-} !Double
    , rG :: {-# UNPACK #-} !Double
    , rB :: {-# UNPACK #-} !Double
    }

-- Operators

infixr 6 `plus`
infixr 7 *^, `mix`

infixl 7 /^

(*^) :: Double -> Color -> Color
(*^) a (Color r g b) = Color (a * r) (a * g) (a * b)

(/^) :: Color -> Double -> Color
(/^) (Color r g b) a = Color (r / a) (g / a) (b / a)

plus :: Color -> Color -> Color
plus (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

-- | Channel-wise product of two colors.
mix :: Color -> Color -> Color
mix (Color r g b) (Color r' g' b') = Color (r * r') (g * g') (b * b')

-- | Channel-wise arithmetic mean of a list of colors.
average :: [Color] -> Color
average [] = black
average cs = foldr plus black cs /^ length' cs
  where
    length' = fromIntegral . length

-- Constants

black :: Color
black = Color 0 0 0

white :: Color
white = Color 1 1 1

blue :: Color
blue = Color 0.5 0.7 1

-- | Linear white-to-blue gradient.
sky :: Vector -> Color
sky d = (1 - t) *^ white `plus` t *^ blue
  where
    t = (vY (unit d) + 1) / 2
