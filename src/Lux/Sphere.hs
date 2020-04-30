{-# LANGUAGE RecordWildCards #-}

module Lux.Sphere
    ( Sphere (..)
    , lambSphere
    , metalSphere
    ) where

import Control.Applicative ((<|>))

import Lux.Color  (Color, mix)
import Lux.Types  (Hit (..), Object (..), Ray (..), at)
import Lux.Vector ((/^), Vector, dot, minus, plus, reflect)


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

lambSphere :: Sphere -> Color -> Object
lambSphere sphere color = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
    Just . Hit t $ \u -> Ray
        { rColor     = mix (rColor ray) color
        , rOrigin    = p
        , rDirection = normal sphere p `plus` u
        }

metalSphere :: Sphere -> Color -> Object
metalSphere sphere color = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
    Just . Hit t $ const Ray
        { rColor     = mix (rColor ray) color
        , rOrigin    = p
        , rDirection = reflect (normal sphere p) (rDirection ray)
        }
