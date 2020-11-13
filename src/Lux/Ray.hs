module Lux.Ray
    ( Ray (..)
    , at
    ) where

import Lux.Vector ((*^), Vector, plus)


data Ray = Ray
    { origin    :: {-# UNPACK #-} !Vector
    , direction :: {-# UNPACK #-} !Vector
    }

at :: Ray -> Double -> Vector
at (Ray p v) t = p `plus` t *^ v
