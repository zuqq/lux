{-# LANGUAGE RecordWildCards #-}

module Lux.Types
    ( Action (..)
    , Hit (..)
    , Material
    , Normal (..)
    , Object
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

data Action = Emit !Color | Scatter !(IO Ray)

data Normal = Normal
    { nPoint  :: !Vector
    , nVector :: !Vector
    }

type Material = Ray -> Normal -> Action

data Hit = Hit !Double !Action

instance Semigroup Hit where
    h @ (Hit t _) <> h' @ (Hit t' _) = if t <= t' then h else h'

type Object = Ray -> Maybe Hit

fromList :: [Object] -> Object
fromList objects ray = foldMap ($ ray) objects
