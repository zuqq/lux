{-# LANGUAGE RecordWildCards #-}

module Lux.Material
    ( Material
    , diffuse
    , specular
    , sphere
    ) where

import Data.Functor ((<&>))

import Lux.Color  (Color)
import Lux.Random (Random, sampleUnitSphere)
import Lux.Ray    (Ray (..), at)
import Lux.Render (Hit (..), Object)
import Lux.Sphere (Sphere (..), normal, time)
import Lux.Vector (Vector, plus, reflect)


type Material
    =  Vector  -- ^ Point of impact.
    -> Vector  -- ^ Direction of the incoming ray.
    -> Vector  -- ^ Surface normal at the point of impact.
    -> Random Ray

diffuse :: Material
diffuse p _ n = sampleUnitSphere <&> \u -> Ray p (n `plus` u)

specular :: Material
specular p v n = pure (Ray p (reflect v n))

-- | Smart constructor for reified 'Sphere's.
sphere
    :: Vector  -- ^ Center.
    -> Double  -- ^ Radius.
    -> Color
    -> Material
    -> Object
sphere center radius color material ray =
    time s ray <&> \t ->
        let p = ray `at` t
            v = direction ray
            n = normal s (ray `at` t) v
        in Hit t color $ material p v n
  where
    s = Sphere {..}
