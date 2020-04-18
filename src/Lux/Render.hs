{-# LANGUAGE RecordWildCards #-}

module Lux.Render where

import Control.Applicative        ((<|>))
import Control.Monad              (replicateM)
import Control.Monad.Random.Class (MonadRandom, getRandomR, getRandomRs)
import Data.List                  (minimumBy)
import Data.Maybe                 (catMaybes)
import Data.Ord                   (comparing)

import Lux.Vector


-- Colors

black :: Vector
black = Vector 0 0 0

white :: Vector
white = Vector 1 1 1

blue :: Vector
blue = Vector 0.5 0.7 1

-- Scenes

data Scene = Scene
    { sWidth      :: !Int
    , sHeight     :: !Int
    , sEye        :: !Vector
    , sLowerLeft  :: !Vector 
    , sHorizontal :: !Vector
    , sVertical   :: !Vector
    , sWorld      :: !Object
    }

-- Rays

data Ray = Ray
    { rOrigin    :: !Vector
    , rDirection :: !Vector
    }

at :: Ray -> Double -> Vector
at Ray {..} t = rOrigin <> t *^ rDirection

-- Hits

data Hit = Hit
    { hTime   :: !Double
    , hPoint  :: !Vector
    , hNormal :: !Vector
    }

-- Objects

newtype Object = Object
    { hit
        :: (Double -> Bool)  -- ^ Range check.
        -> Ray
        -> Maybe Hit
    }

fromList :: [Object] -> Object
fromList objs = Object $ \range ray -> do
    hs <- invert [ hit obj range ray | obj <- objs ]
    return . minimumBy (comparing hTime) $ hs
  where
    invert :: [Maybe a] -> Maybe [a]
    invert mas = case catMaybes mas of
        [] -> Nothing
        as -> Just as

-- Spheres

mkSphere
    :: Vector  -- ^ Center
    -> Double  -- ^ Radius
    -> Object
mkSphere center radius = Object $ \check ray@Ray {..} -> do
    let oc = rOrigin `minus` center
    let a  = dot rDirection rDirection
    let b  = dot oc rDirection
    let c  = dot oc oc - radius * radius
    let disc = b * b - a * c
    root <- safeSqrt disc
    let t  = (-b - root) / a
    let t' = (-b + root) / a
    mkHit check ray t <|> mkHit check ray t'
  where
    safeSqrt x
        | x < 0     = Nothing
        | otherwise = Just (sqrt x)
    mkHit check ray t
        | not (check t) = Nothing
        | otherwise     = Just $ Hit t p n
      where
        p = ray `at` t
        n = (p `minus` center) /^ radius

-- Rendering

randUnit :: MonadRandom m => m Vector
randUnit = do
    a <- getRandomR (0, 2 * pi)
    z <- getRandomR (-1, 1)
    let r = sqrt $ 1 - z * z
    return $ Vector (r * cos a) (r * sin a) z

rayColor
    :: MonadRandom m
    => Object
    -> Ray
    -> Int            -- ^ Depth
    -> m Vector
rayColor world ray@(Ray _ d) depth
    | depth <= 0 = return black
    | otherwise  = case hit world (>= 0.001) ray of
        Nothing       -> return $ (1 - t) *^ white <> t *^ blue
        Just Hit {..} -> do
            n <- randUnit
            let ray' = Ray hPoint (hNormal <> n)
            c <- rayColor world ray' (depth - 1)
            return $ 0.5 *^ c
  where
    t = 0.5 * (y (unit d) + 1.0)

pxColor :: MonadRandom m => Scene -> Int -> Int -> m Vector
pxColor scene@Scene {..} i j = do
    eu <- getRandomR (-1, 1) 
    ev <- getRandomR (-1, 1) 
    let u = (fromIntegral i + eu) / fromIntegral sWidth
    let v = (fromIntegral j + ev) / fromIntegral sHeight
    let ray = Ray sEye (sLowerLeft <> u *^ sHorizontal <> v *^ sVertical)
    rayColor sWorld ray 50

-- Antialiasing

withAA :: MonadRandom m => Scene -> Int -> Int -> m Vector
withAA scene i j = (/^ 100) . mconcat <$> replicateM 100 (pxColor scene i j)
