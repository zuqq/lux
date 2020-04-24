{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Render
    ( Object
    , Ray (..)
    , fromList
    , lambSphere
    , metalSphere
    , Sphere (..)
    , sample
    , white
    ) where

import Control.Applicative        ((<|>))
import Control.Monad.Random.Class (MonadRandom, getRandomR)

import Lux.Vector ((*^), (/^), Vector (..), dot, minus, plus, prod, unit)


black :: Vector
black = Vector 0 0 0

white :: Vector
white = Vector 1 1 1

blue :: Vector
blue = Vector 0.5 0.7 1

-- | Linear white-to-blue gradient.
sky :: Ray -> Vector
sky (Ray _ _ d) = (1 - t) *^ white `plus` t *^ blue
  where
    t = (vY (unit d) + 1) / 2

-- Rays

data Ray = Ray
    { rColor     :: !Vector
    , rOrigin    :: !Vector
    , rDirection :: !Vector
    }
    deriving (Read, Show)

at :: Ray -> Double -> Vector
at Ray {..} t = rOrigin `plus` t *^ rDirection

-- Hits

data Hit = Hit
    { hTime    :: !Double
    , hScatter :: !(Vector -> Ray)
    }

instance Semigroup Hit where
    h <> h' = if hTime h < hTime h' then h else h'

-- Objects

newtype Object = Object { hit :: Ray -> Maybe Hit }

fromList :: [Object] -> Object
fromList objs = Object $ \ray -> foldMap (`hit` ray) objs

-- Spheres

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

lambSphere :: Sphere -> Vector -> Object
lambSphere sphere color = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
    Just . Hit t $ \u -> Ray
        { rColor     = rColor ray `prod` color
        , rOrigin    = p
        , rDirection = normal sphere p `plus` u
        }

reflect :: Vector -> Vector -> Vector
reflect n v = v `minus` (2 *^ dot n v *^ n)

metalSphere :: Sphere -> Vector -> Object
metalSphere sphere color = Object $ \ray -> do
    t <- time sphere ray
    let p = ray `at` t
    Just . Hit t $ const Ray
        { rColor     = rColor ray `prod` color
        , rOrigin    = p
        , rDirection = reflect (normal sphere p) (rDirection ray)
        }

-- Rendering

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
    -> m Vector
bounce world = go 50
  where
    go k !acc = acc >>= \ray -> if k <= 0
        then return $ rColor ray `prod` black
        else case hit world ray of
            Nothing        -> return $ rColor ray `prod` sky ray
            Just (Hit _ f) -> go (k - 1) (f <$> randUnit)

-- Antialiasing

sample
    :: MonadRandom m
    => Object         -- ^ World.
    -> m Ray          -- ^ Target.
    -> m Vector
sample world mray = (/^ 100) <$> go 100 (return $ Vector 0 0 0)
  where
    go k !acc = if k <= 0
        then acc
        else go (k - 1) (plus <$> bounce world mray <*> acc)
