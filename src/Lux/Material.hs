{-# LANGUAGE ViewPatterns #-}

module Lux.Material
    ( Action (..)
    , Material
    , diffuse
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

randUnit :: StdGen -> (Vector, StdGen)
randUnit g =
    let (a, g')  = uniformR (0, 2 * pi) g
        (z, g'') = uniformR (-1, 1) g'
        r = sqrt $ 1 - z * z
    in (Vector (r * cos a) (r * sin a) z, g'')

diffuse :: Color -> Material
diffuse c c' _ p n = Scatter $ \g ->
    let (u, g') = randUnit g
    in (Ray (mix c c') p (n `plus` u), g')

reflective :: Color -> Material
reflective c c' (unit -> v) p n = Scatter . (,) $
    Ray (mix c c') p (reflect v n)
