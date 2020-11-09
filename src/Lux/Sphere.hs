{-# LANGUAGE RecordWildCards #-}

module Lux.Sphere
    ( Sphere (..)
    , time
    , normal
    ) where

import Control.Applicative ((<|>))

import Lux.Ray    (Ray (..))
import Lux.Vector ((*^), Vector (..), dot, minus, unit)


data Sphere = Sphere
    { center :: !Vector
    , radius :: !Double
    }

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

-- | Unit normal at the point of impact, pointing in the same direction.
normal
    :: Sphere
    -> Vector  -- ^ Point of impact.
    -> Vector  -- ^ Direction of the incoming ray.
    -> Vector
normal Sphere {..} p v = unit $ a *^ n
  where
    n = p `minus` center
    a = if dot v n > 0 then (-1) else 1
