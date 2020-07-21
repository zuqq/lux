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
    , average
    , gradient
    , mix
    ) where

import Lux.Pair (Pair (..))


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

infixr 6 `plus`
infixr 7 *^, `mix`

infixl 7 /^

(*^) :: Double -> Color -> Color
(*^) a (Color r g b) = Color (a * r) (a * g) (a * b)

(/^) :: Color -> Double -> Color
(/^) (Color r g b) a = Color (r / a) (g / a) (b / a)

plus :: Color -> Color -> Color
plus (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

average :: Int -> (a -> (Color, a)) -> a -> (Color, a)
average n f x = go n (Pair black x)
  where
    go k (Pair c g) = if k <= 0
        then (c /^ fromIntegral n, g)
        else go (k - 1) $! let (c', g') = f g in Pair (c `plus` c') g'

gradient :: Color -> Color -> Double -> Color
gradient x y t = (1 - t) *^ x `plus` t *^ y

-- | Channel-wise product of two colors.
mix :: Color -> Color -> Color
mix (Color r g b) (Color r' g' b') = Color (r * r') (g * g') (b * b')
