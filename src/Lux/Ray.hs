module Lux.Ray
    ( Ray (..)
    , at
    ) where

import Lux.Color  (Color)
import Lux.Vector ((*^), Vector, plus)


data Ray = Ray
    { color     :: {-# UNPACK #-} !Color
    , origin    :: {-# UNPACK #-} !Vector
    , direction :: {-# UNPACK #-} !Vector
    }

at :: Ray -> Double -> Vector
at (Ray _ p v) t = p `plus` t *^ v
