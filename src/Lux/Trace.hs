{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( lambSphere
    , metalSphere
    , sample
    ) where

import Control.Applicative        ((<|>))
import Control.Monad              (replicateM)
import Control.Monad.Random.Class (MonadRandom, getRandomR)

import Lux.Color  (average, black, Color, mix, sky)
import Lux.Types  (Hit (..), Object (..), Ray (..), Sphere (..))
import Lux.Vector ((*^), (/^), Vector (..), dot, minus, plus)


-- Spheres

at :: Ray -> Double -> Vector
at Ray {..} t = rOrigin `plus` t *^ rDirection

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

reflect :: Vector -> Vector -> Vector
reflect n v = v `minus` (2 *^ dot n v *^ n)

metalSphere :: Sphere -> Color -> Object
metalSphere sphere color = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
    Just . Hit t $ const Ray
        { rColor     = mix (rColor ray) color
        , rOrigin    = p
        , rDirection = reflect (normal sphere p) (rDirection ray)
        }

-- Ray tracing

randUnit :: MonadRandom m => m Vector
randUnit = do
    a <- getRandomR (0, 2 * pi)
    z <- getRandomR (-1, 1)
    let r = sqrt $ 1 - z * z
    return $ Vector (r * cos a) (r * sin a) z

bounce
    :: MonadRandom m
    => Object
    -> m Ray
    -> m Color
bounce world = go 50
  where
    go k !acc = acc >>= \ray@Ray {..} -> if k <= 0
        then return $ mix rColor black
        else case hit world ray of
            Nothing        -> return $ mix rColor (sky rDirection)
            Just (Hit _ f) -> go (k - 1) (f <$> randUnit)

sample
    :: MonadRandom m
    => Object         -- ^ World.
    -> m Ray          -- ^ Target.
    -> m Color
sample world mray = average <$> replicateM 100 (bounce world mray)
