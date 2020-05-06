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

data Hit m = Hit !Double !(m Ray)

instance Semigroup (Hit m) where
    h@(Hit t _) <> h'@(Hit t' _) = if t < t' then h else h'

type Object m = Ray -> Maybe (Hit m)

fromList :: [Object m] -> Object m
fromList objs ray = foldMap ($ ray) objs
