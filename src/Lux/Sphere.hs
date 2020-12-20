{-# LANGUAGE RecordWildCards #-}

module Lux.Sphere
    ( Sphere (..)
    , normal
    , time
    )
    where

import Control.Applicative ((<|>))

import Lux.Ray    (Ray (..))
import Lux.Vector (Vector (..), dot, minus, unit)

data Sphere = Sphere
    { center :: {-# UNPACK #-} !Vector
    , radius :: {-# UNPACK #-} !Double
    }

-- | Compute the smallest @t :: Double@ at which the 'Ray' hits the 'Sphere'.
time :: Sphere -> Ray -> Maybe Double
time Sphere {..} Ray {..} =
    let oc = origin `minus` center
        a  = dot direction direction
        b  = dot oc direction
        c  = dot oc oc - radius * radius
        disc = b * b - a * c
    in safeSqrt disc >>= \root ->
        let t  = (-b - root) / a
            t' = (-b + root) / a
        in safeTime t <|> safeTime t'
  where
    safeSqrt x
        | x < 0     = Nothing
        | otherwise = Just $ sqrt x
    safeTime t
        | t < 0.001 = Nothing
        | otherwise = Just t

-- | @normal sphere p@ is the outward-facing unit normal of @sphere@ at @p@.
normal :: Sphere -> Vector -> Vector
normal Sphere {..} p = unit $ p `minus` center
