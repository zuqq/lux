-- | Floating-point representation of RGB colors.
--
-- The 'Color' type is isomorphic to 'Lux.Vector.Vector'; it is distinguished
-- only because it needs a different set of combinators (e.g., 'mix').
module Lux.Color
    (
    -- * Type
      Color (..)
    -- * Constants
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

-- | Floating-point representation of RGB colors.
data Color = Color
    {-# UNPACK #-} !Double
    -- ^ R
    {-# UNPACK #-} !Double
    -- ^ G
    {-# UNPACK #-} !Double
    -- ^ B

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

-- | Channel-wise multiplication with a scalar.
(*~) :: Double -> Color -> Color
(*~) a (Color r g b) = Color (a * r) (a * g) (a * b)

-- | Channel-wise sum of two colors.
(~+~) :: Color -> Color -> Color
(~+~) (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

-- | @gradient x y@ is a linear gradient from @x@ to @y@.
gradient :: Color -> Color -> Double -> Color
gradient x y t = (1 - t) *~ x ~+~ t *~ y

-- | Channel-wise product of two colors.
mix :: Color -> Color -> Color
mix (Color r g b) (Color r' g' b') = Color (r * r') (g * g') (b * b')
