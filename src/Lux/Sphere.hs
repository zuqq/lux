{-# LANGUAGE RecordWildCards #-}

module Lux.Sphere
    ( Sphere (..)
    , glassSphere
    , lambSphere
    , metalSphere
    ) where

import Control.Applicative ((<|>))

import Lux.Color  (Color, mix)
import Lux.Types  (Hit (..), Object (..), Ray (..), at)
import Lux.Vector ((*^), (/^), Vector, dot, minus, plus, unit)


-- | Data type describing an abstract sphere; passed to the constructors for the
-- actual 'Object's.
data Sphere = Sphere
    { sCenter :: !Vector
    , sRadius :: !Double
    }

time :: Sphere -> Ray -> Maybe Double
time Sphere {..} Ray {..} =
    let oc = rOrigin `minus` sCenter
        a  = dot rDirection rDirection
        b  = dot oc rDirection
        c  = dot oc oc - sRadius * sRadius
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

normal :: Sphere -> Vector -> Vector
normal Sphere {..} p = (p `minus` sCenter) /^ sRadius

reflect
    :: Vector  -- ^ Unit normal at the point of impact.
    -> Vector  -- ^ Unit vector to reflect.
    -> Vector
reflect n v = v `minus` (2 *^ dot n v *^ n)

refract
    :: Vector  -- ^ Unit normal at the point of impact.
    -> Vector  -- ^ Unit vector to reflect.
    -> Double  -- ^ Quotient of the refractive indices.
    -> Vector
refract n v ix = par `plus` perp
  where
    par  = ix *^ (v `minus` dot v n *^ n)
    perp = (-sqrt (1 - dot par par)) *^ n

-- | A solid glass sphere.
glassSphere
    :: Sphere
    -> Double  -- ^ Refractive index.
    -> Object
glassSphere sphere ix = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
        n = normal sphere p
        v = unit (rDirection ray)
        -- The vector @n@ always points outside, so we need to invert it if the
        -- ray is coming from the inside.
        (ix', n') = if dot v n > 0 then (ix, (-1) *^ n) else (1 / ix, n)
        -- Schlick approximation
        u  = - dot v n'
        f0 = (ix' - 1) ^ 2 / (ix' + 1) ^ 2
        f  = f0 + (1 - f0) * (1 - u) ^ 5
    return . Hit t $ \_ x -> Ray
        { rColor     = rColor ray
        , rOrigin    = p
        -- Reflect with probability @f@ or if Snell's law doesn't apply.
        , rDirection = if x < f || ix' * sqrt (1 - u ^ 2) > 1
            then reflect n' v
            else refract n' v ix'
        }

-- | A diffuse sphere.
lambSphere :: Sphere -> Color -> Object
lambSphere sphere color = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
    return . Hit t $ \u _ -> Ray
        { rColor     = mix (rColor ray) color
        , rOrigin    = p
        , rDirection = normal sphere p `plus` u
        }

-- | A smooth metal sphere.
metalSphere :: Sphere -> Color -> Object
metalSphere sphere color = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
    return . Hit t $ \_ _ -> Ray
        { rColor     = mix (rColor ray) color
        , rOrigin    = p
        , rDirection = reflect (normal sphere p) (rDirection ray)
        }
