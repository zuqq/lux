{-# LANGUAGE RecordWildCards #-}

module Lux.Types
    ( Hit (..)
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

data Hit = Hit
    { hTime    :: !Double
    , hScatter :: !(Vector -> Double -> Ray)
    }

instance Semigroup Hit where
    h <> h' = if hTime h < hTime h' then h else h'

newtype Object = Object { hit :: Ray -> Maybe Hit }

fromList :: [Object] -> Object
fromList objs = Object $ \ray -> foldMap (`hit` ray) objs
