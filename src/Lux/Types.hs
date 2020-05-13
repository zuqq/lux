{-# LANGUAGE RecordWildCards #-}

module Lux.Types
    ( Hit (..)
    , Material (..)
    , Object (..)
    , Ray (..)
    , at
    , fromList
    ) where

import Lux.Color  (Color)
import Lux.Vector ((*^), Vector, plus)


data Ray = Ray
    { rColor     :: !Color
    , rOrigin    :: !Vector
    , rDirection :: !Vector
    }

at :: Ray -> Double -> Vector
at Ray {..} t = rOrigin `plus` t *^ rDirection

data Material
    = Dielectric !Double
    | Diffuse    !Color
    | Light      !Color
    | Reflective !Color

data Hit = Hit
    { hTime     :: !Double
    , hPoint    :: !Vector
    , hNormal   :: !Vector
    , hMaterial :: !Material
    }

instance Semigroup Hit where
    h <> h' = if hTime h <= hTime h' then h else h'

newtype Object = Object {hit :: Ray -> Maybe Hit}

fromList :: [Object] -> Object
fromList objects = Object $ \ray -> foldMap (\x -> hit x ray) objects
