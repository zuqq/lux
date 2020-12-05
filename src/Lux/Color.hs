module Lux.Color
    (
    -- * Type
      Color (..)
    -- * Colors
    , black
    , blue
    , glacier
    , gray
    , green
    , navy
    , white
    -- * Operators
    , (*~)
    , (~+~)
    , gradient
    , mix
    )
    where


data Color = Color
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double

black :: Color
black = Color 0 0 0

gray :: Color
gray = Color 0.5 0.5 0.5

white :: Color
white = Color 1 1 1

-- | PANTONE 276 C
navy :: Color
navy = Color 0.125 0.11 0.243

-- | PANTONE 2707 C
blue :: Color
blue = Color 0.765 0.843 0.933

-- | PANTONE 15-6437 TCX
green :: Color
green = Color 0.482 0.702 0.412

-- | PANTONE Glacier Gray
glacier :: Color
glacier = Color 0.773 0.777 0.780

infixr 6 ~+~
infixr 7 *~, `mix`

(*~) :: Double -> Color -> Color
(*~) a (Color r g b) = Color (a * r) (a * g) (a * b)

(~+~) :: Color -> Color -> Color
(~+~) (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

gradient :: Color -> Color -> Double -> Color
gradient x y t = (1 - t) *~ x ~+~ t *~ y

-- | Channel-wise product of two colors.
mix :: Color -> Color -> Color
mix (Color r g b) (Color r' g' b') = Color (r * r') (g * g') (b * b')
