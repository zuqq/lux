{-# LANGUAGE RecordWildCards #-}

module Lux.Types
    ( Action (..)
    , Hit (..)
    , Material
    , Object
    , Ray (..)
    , at
    , fromList
    ) where

import Lux.Color  (Color)
import Lux.Random (Random)
import Lux.Vector ((*^), Vector, plus)


data Ray = Ray
    { rColor     :: !Color
    , rOrigin    :: !Vector
    , rDirection :: !Vector
    }

at :: Ray -> Double -> Vector
at Ray {..} t = rOrigin `plus` t *^ rDirection

data Action = Emit !Color | Scatter !(Random Ray)

type Material
    =  Color   -- Color of the incoming ray.
    -> Vector  -- Direction of the incoming ray.
    -> Vector  -- Point of impact.
    -> Vector  -- Unit normal at the point of impact.
    -> Action

data Hit = Hit !Double !Action

instance Semigroup Hit where
    h @ (Hit t _) <> h' @ (Hit t' _) = if t <= t' then h else h'

type Object = Ray -> Maybe Hit

fromList :: [Object] -> Object
fromList objects ray = foldMap ($ ray) objects
