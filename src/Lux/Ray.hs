module Lux.Ray
    ( Ray (..)
    , at
    )
    where

import Lux.Vector

-- | A ray in three-dimensional space.
data Ray = Ray
    { origin    :: {-# UNPACK #-} !Vector
    , direction :: {-# UNPACK #-} !Vector
    }

-- | @ray `at` t@ is the point in space that @ray@ reaches at time @t@.
at :: Ray -> Double -> Vector
at (Ray p v) t = p `plus` t *^ v
