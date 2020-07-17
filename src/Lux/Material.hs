{-# LANGUAGE RecordWildCards #-}

module Lux.Material
    ( dielectric
    , diffuse
    , light
    , reflective
    ) where

import Data.Functor  ((<&>))
import System.Random (randomIO, randomRIO)

import Lux.Color  (Color, mix)
import Lux.Types  (Action (..), Normal (..), Ray (..))
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

dielectric :: Double -> Ray -> Normal -> Action
dielectric ix Ray {..} Normal {..} = Scatter $
    randomIO <&> \x -> Ray
        { rColor     = rColor
        , rOrigin    = nPoint
        -- Reflect with probability @f@ or if Snell's law doesn't apply.
        , rDirection = if x < f || ix' * sqrt (1 - u ^ (2 :: Int)) > 1
            then reflect nVector v
            else refract nVector v ix'
        }
  where
    v   = unit rDirection
    ix' = if dot v nVector > 0 then ix else 1 / ix
    -- Schlick approximation.
    u   = -dot v nVector
    f0  = (ix' - 1) ^ (2 :: Int) / (ix' + 1) ^ (2 :: Int)
    f   = f0 + (1 - f0) * (1 - u) ^ (5 :: Int)

diffuse :: Color -> Ray -> Normal -> Action
diffuse color Ray {..} Normal {..} = Scatter $
    randomUnit <&> \u -> Ray
        { rColor     = mix rColor color
        , rOrigin    = nPoint
        , rDirection = nVector `plus` u
        }

light :: Color -> Ray -> Normal -> Action
light color _ _ = Emit color

reflective :: Color -> Ray -> Normal -> Action
reflective color Ray {..} Normal {..} = Scatter $
    return Ray
        { rColor     = mix rColor color
        , rOrigin    = nPoint
        , rDirection = reflect nVector rDirection
        }
