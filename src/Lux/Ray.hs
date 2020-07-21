module Lux.Ray
    ( Ray (..)
    , at
    ) where

import Lux.Color  (Color)
import Lux.Vector ((*^), Vector, plus)


data Ray = Ray
    { rColor     :: {-# UNPACK #-} !Color
    , rOrigin    :: {-# UNPACK #-} !Vector
    , rDirection :: {-# UNPACK #-} !Vector
    }

at :: Ray -> Double -> Vector
at (Ray _ p v) t = p `plus` t *^ v
