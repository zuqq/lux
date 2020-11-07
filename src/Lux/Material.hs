{-# LANGUAGE ViewPatterns #-}

module Lux.Material
    ( Material
    , diffuse
    , reflective
    ) where

import System.Random (StdGen, uniformR)

import Lux.Color  (Color)
import Lux.Ray    (Ray (..))
import Lux.Vector (Vector (..), plus, reflect, unit)


type Material
    =  Vector  -- ^ Direction of the incoming ray.
    -> Vector  -- ^ Point of impact.
    -> Vector  -- ^ Unit normal at the point of impact.
    -> StdGen -> ((Color, Ray), StdGen)

randUnit :: StdGen -> (Vector, StdGen)
randUnit g =
    let (a, g')  = uniformR (0, 2 * pi) g
        (z, g'') = uniformR (-1, 1) g'
        r = sqrt $ 1 - z * z
    in (Vector (r * cos a) (r * sin a) z, g'')

diffuse :: Color -> Material
diffuse c _ p n g = let (u, g') = randUnit g in ((c, Ray p (n `plus` u)), g')

reflective :: Color -> Material
reflective c (unit -> v) p n = (,) $ (c, Ray p (reflect v n))
