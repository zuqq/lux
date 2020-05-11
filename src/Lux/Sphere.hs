{-# LANGUAGE RecordWildCards #-}

module Lux.Sphere
    ( Sphere (..)
    , diffuse
    , glass
    , light
    , metal
    ) where

import Control.Applicative        ((<|>))
import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)

import Lux.Color  (Color, mix)
import Lux.Types  (Hit (..), Object, Ray (..), at)
import Lux.Vector ((*^), (/^), Vector (..), dot, minus, plus, unit)


infixr 2 <&>

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

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
    -> Vector  -- ^ Direction of the incoming ray.
    -> Vector  -- ^ Point of contact.
    -> Vector
normal Sphere {..} v p = unit $ if dot v n > 0 then (-1) *^ n else n
  where
    n = p `minus` sCenter

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

-- | A solid glass sphere.
glass
    :: MonadRandom m
    => Sphere
    -> Double         -- ^ Refractive index.
    -> Object m
glass sphere ix ray = do
    t <- time sphere ray
    let p   = ray `at` t
        v   = unit (rDirection ray)
        n   = normal sphere v p
        ix' = if dot v n > 0 then ix else 1 / ix
        -- Schlick approximation
        u  = -dot v n
        f0 = (ix' - 1) ^ 2 / (ix' + 1) ^ 2
        f  = f0 + (1 - f0) * (1 - u) ^ 5
    return . Hit t $ getRandom <&> \x -> Ray
        { rColor     = rColor ray
        , rOrigin    = p
        -- Reflect with probability @f@ or if Snell's law doesn't apply.
        , rDirection = if x < f || ix' * sqrt (1 - u ^ 2) > 1
            then reflect n v
            else refract n v ix'
        }

randUnit :: MonadRandom m => m Vector
randUnit = do
    a <- getRandomR (0, 2 * pi)
    z <- getRandomR (-1, 1)
    let r = sqrt $ 1 - z * z
    return $ Vector (r * cos a) (r * sin a) z

-- | A diffuse sphere.
diffuse :: MonadRandom m => Sphere -> Color -> Object m
diffuse sphere color ray@Ray {..} = do
    t <- time sphere ray
    let p = ray `at` t
    return . Hit t $ randUnit <&> \u -> Ray
        { rColor     = mix rColor color
        , rOrigin    = p
        , rDirection = normal sphere rDirection p `plus` u
        }

-- | A smooth metal sphere.
metal :: Monad m => Sphere -> Color -> Object m
metal sphere color ray@Ray {..} = do
    t <- time sphere ray
    let p = ray `at` t
    return . Hit t $ return Ray
        { rColor     = mix rColor color
        , rOrigin    = p
        , rDirection = reflect (normal sphere rDirection p) rDirection
        }

-- | A spherical light.
light :: Monad m => Sphere -> Color -> Object m
light sphere color ray = do
    t <- time sphere ray
    let p = ray `at` t
    return . Hit t $ return Ray
        { rColor     = color
        , rOrigin    = p
        , rDirection = Vector 0 0 0
        }
