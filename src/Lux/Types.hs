module Lux.Types
    ( Hit (..)
    , Object (..)
    , Ray (..)
    , Sphere (..)
    , fromList
    ) where

import Lux.Color  (Color)
import Lux.Vector (Vector)


data Ray = Ray
    { rColor     :: !Color
    , rOrigin    :: !Vector
    , rDirection :: !Vector
    }

data Hit = Hit
    { hTime    :: !Double
    , hScatter :: !(Vector -> Ray)
    }

instance Semigroup Hit where
    h <> h' = if hTime h < hTime h' then h else h'

newtype Object = Object { hit :: Ray -> Maybe Hit }

fromList :: [Object] -> Object
fromList objs = Object $ \ray -> foldMap (`hit` ray) objs

data Sphere = Sphere
    { sCenter :: !Vector
    , sRadius :: !Double
    }
