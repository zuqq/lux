module Lux.Color
    (
    -- * Type
      Color (..)
    -- * Colors
    , black
    , gray
    , navy
    , white
    -- * Operators
    , (*~)
    , (~+~)
    , gradient
    , mix
    ) where


-- Type ------------------------------------------------------------------------

data Color = Color
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double

-- Colors ----------------------------------------------------------------------

black :: Color
black = Color 0 0 0

gray :: Color
gray = Color 0.5 0.5 0.5

white :: Color
white = Color 1 1 1

-- | PANTONE 276 C
navy :: Color
navy = Color 0.125 0.11 0.243

-- Operators -------------------------------------------------------------------

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
