{-# LANGUAGE ViewPatterns #-}

module Lux.Material
    ( Action (..)
    , Material
    , dielectric
    , diffuse
    , light
    , reflective
    ) where

import System.Random (StdGen, uniformR)

import Lux.Color  (Color, mix)
import Lux.Ray    (Ray (..))
import Lux.Vector ((*^), Vector (..), dot, minus, plus, unit)


data Action = Emit !Color | Scatter !(StdGen -> (Ray, StdGen))

type Material
    =  Color   -- ^ Color of the incoming ray.
    -> Vector  -- ^ Direction of the incoming ray.
    -> Vector  -- ^ Point of impact.
    -> Vector  -- ^ Unit normal at the point of impact.
    -> Action

reflect
    :: Vector  -- ^ Unit vector to reflect.
    -> Vector  -- ^ Unit normal at the point of impact.
    -> Vector
reflect v n = v `minus` (2 *^ dot n v *^ n)

refract
    :: Vector  -- ^ Unit vector to reflect.
    -> Vector  -- ^ Unit normal at the point of impact.
    -> Double  -- ^ Quotient of the refractive indices.
    -> Vector
refract v n ix =
    let par = ix *^ (v `minus` dot v n *^ n)
    in par `plus` (-sqrt (1 - dot par par)) *^ n

dielectric :: Double -> Material
dielectric ix color (unit -> v) p n = Scatter $ \g ->
    let ix' = if dot v n > 0 then ix else 1 / ix
        -- Schlick approximation.
        u   = -dot v n
        f0  = (ix' - 1) ^ (2 :: Int) / (ix' + 1) ^ (2 :: Int)
        f   = f0 + (1 - f0) * (1 - u) ^ (5 :: Int)
        (x, g') = uniformR (0, 1) g
        -- Reflect with probability @f@ or if Snell's law doesn't apply.
        v'  = if x < f || ix' * sqrt (1 - u ^ (2 :: Int)) > 1
            then reflect v n
            else refract v n ix'
    in (Ray color p v', g')

randUnit :: StdGen -> (Vector, StdGen)
randUnit g =
    let (a, g')  = uniformR (0, 2 * pi) g
        (z, g'') = uniformR (-1, 1) g'
        r = sqrt $ 1 - z * z
    in (Vector (r * cos a) (r * sin a) z, g'')

diffuse :: Color -> Material
diffuse color color' _ p n = Scatter $ \g ->
    let (u, g') = randUnit g
    in (Ray (mix color color') p (n `plus` u), g')

light :: Color -> Material
light color _ _ _ _ = Emit color

reflective :: Color -> Material
reflective color color' (unit -> v) p n = Scatter . (,) $
    Ray (mix color color') p (reflect v n)
