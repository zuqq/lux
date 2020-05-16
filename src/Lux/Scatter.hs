{-# LANGUAGE RecordWildCards #-}

module Lux.Scatter
    ( scatter
    ) where

import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)

import Lux.Color  (Color, mix)
import Lux.Types  (Hit (..), Material (..), Ray (..))
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

randUnit :: MonadRandom m => m Vector
randUnit = do
    a <- getRandomR (0, 2 * pi)
    z <- getRandomR (-1, 1)
    let r = sqrt $ 1 - z * z
    return $ Vector (r * cos a) (r * sin a) z

scatter
    :: MonadRandom m
    => Ray
    -> Hit
    -> Either Color (m Ray)
scatter Ray {..} Hit {..} = case hMaterial of
    Dielectric ix    -> Right $ do
        x <- getRandom
        let v   = unit rDirection
            ix' = if dot v hNormal > 0 then ix else 1 / ix
            -- Schlick approximation.
            u   = -dot v hNormal
            f0  = (ix' - 1) ^ (2 :: Int) / (ix' + 1) ^ (2 :: Int)
            f   = f0 + (1 - f0) * (1 - u) ^ (5 :: Int)
        return Ray
            { rColor     = rColor
            , rOrigin    = hPoint
            -- Reflect with probability @f@ or if Snell's law doesn't apply.
            , rDirection = if x < f || ix' * sqrt (1 - u ^ (2 :: Int)) > 1
                then reflect hNormal v
                else refract hNormal v ix'
            }
    Diffuse color    -> Right $ do
        u <- randUnit
        return Ray
            { rColor     = mix rColor color
            , rOrigin    = hPoint
            , rDirection = hNormal `plus` u
            }
    Light color      -> Left color
    Reflective color -> Right $ return Ray
        { rColor     = mix rColor color
        , rOrigin    = hPoint
        , rDirection = reflect hNormal rDirection
        }
