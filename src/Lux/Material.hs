{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Lux.Material
    ( dielectric
    , diffuse
    , light
    , reflective
    ) where

import Data.Functor  ((<&>))
import System.Random (randomIO, randomRIO)

import Lux.Color  (Color, mix)
import Lux.Types  (Action (..), Material, Ray (..))
import Lux.Vector ((*^), Vector (..), dot, minus, plus, unit)


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

randomUnit :: IO Vector
randomUnit = do
    a <- randomRIO (0, 2 * pi)
    z <- randomRIO (-1, 1)
    let r = sqrt $ 1 - z * z
    return $ Vector (r * cos a) (r * sin a) z

dielectric :: Double -> Material
dielectric ix rColor (unit -> v) p n = Scatter $
    randomIO <&> \x -> Ray
        { rColor     = rColor
        , rOrigin    = p
        -- Reflect with probability @f@ or if Snell's law doesn't apply.
        , rDirection = if x < f || ix' * sqrt (1 - u ^ (2 :: Int)) > 1
            then reflect n v
            else refract n v ix'
        }
  where
    ix' = if dot v n > 0 then ix else 1 / ix
    -- Schlick approximation.
    u   = -dot v n
    f0  = (ix' - 1) ^ (2 :: Int) / (ix' + 1) ^ (2 :: Int)
    f   = f0 + (1 - f0) * (1 - u) ^ (5 :: Int)

diffuse :: Color -> Material
diffuse color rColor _ p n = Scatter $
    randomUnit <&> \u -> Ray
        { rColor     = mix rColor color
        , rOrigin    = p
        , rDirection = n `plus` u
        }

light :: Color -> Material
light color _ _ _ _ = Emit color

reflective :: Color -> Material
reflective color rColor (unit -> v) p n = Scatter $
    return Ray
        { rColor     = mix rColor color
        , rOrigin    = p
        , rDirection = reflect n v
        }
