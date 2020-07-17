module Lux.Color
    ( (*^)
    , (/^)
    , Color (..)
    , black
    , mix
    , navy
    , plus
    , white
    ) where


data Color = Color
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double

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

black :: Color
black = Color 0 0 0

white :: Color
white = Color 1 1 1

-- | PANTONE 276 C
navy :: Color
navy = Color 0.125 0.11 0.243
