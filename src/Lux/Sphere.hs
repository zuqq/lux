{-# LANGUAGE RecordWildCards #-}

module Lux.Sphere
    ( Sphere (..)
    , withMaterial
    ) where

import Control.Applicative ((<|>))
import Data.Functor        ((<&>))

import Lux.Types  (Hit (..), Material, Object, Ray (..), at)
import Lux.Vector ((*^), Vector (..), dot, minus, unit)


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

normal
    :: Sphere
    -> Vector  -- ^ Drection of the incoming ray.
    -> Vector  -- ^ Point of contact.
    -> Vector
normal Sphere {..} v p = unit $ a *^ n
  where
    n = p `minus` sCenter
    a = if dot v n > 0 then (-1) else 1

withMaterial :: Material -> Sphere -> Object
withMaterial material sphere ray @ Ray {..} =
    time sphere ray <&> \t -> let p = ray `at` t
        in Hit t (material rColor rDirection p (normal sphere rDirection p))
