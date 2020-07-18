{-# LANGUAGE RecordWildCards #-}

module Lux.Render
    ( Picture (..)
    , header
    , render
    , serialize
    ) where

import Lux.Color  (Color (..), white)
import Lux.Random (Random, sample)
import Lux.Trace  (hit)
import Lux.Types  (Object, Ray (..))
import Lux.Vector ((*^), (/^), Vector (..), cross, len, minus, plus, unit)


-- | Plain PPM header, see <http://netpbm.sourceforge.net/doc/ppm.html>.
header
    :: Int     -- ^ Width
    -> Int     -- ^ Height
    -> String
header w h = unwords ["P3", show w, show h, "255"]

serialize :: Color -> String
serialize (Color r g b) = unwords $
    show . (floor :: Double -> Int) . (255.999 *) <$> [r, g, b]

data Picture = Picture
    { pLens   :: !Vector  -- ^ Center of the lens.
    , pFocus  :: !Vector  -- ^ Center of the focal plane.
    , pUp     :: !Vector  -- ^ "Up" direction.
    , pAngle  :: !Double  -- ^ Angle of view.
    , pWidth  :: !Int     -- ^ Width in pixels.
    , pHeight :: !Int     -- ^ Height in pixels.
    , pApert  :: !Double  -- ^ Aperture.
    }

randPolar :: Double -> Random (Double, Double)
randPolar maxRadius = do
    a <- sample (0, 2 * pi)
    r <- sample (0, maxRadius)
    return (r * cos a, r * sin a)

shoot
    :: Picture
    -> Double      -- ^ Row in fractional pixels.
    -> Double      -- ^ Column in fractional pixels.
    -> Random Ray
shoot Picture {..} fRow fCol = do
    (dx, dy) <- randPolar (pApert / 2)
    let offset = dx *^ ex `plus` dy *^ ey
    return Ray
        { rColor     = white
        , rOrigin    = pLens `plus` offset
        , rDirection = (c `plus` x *^ ex `plus` y *^ ey) `minus` offset
        }
  where
    a /. b  = a / fromIntegral b
    a ./. b = fromIntegral a / fromIntegral b

    d  = pLens `minus` pFocus
    ez = unit d
    z  = len d
    h  = 2 * z * tan (pAngle / 2)
    w  = pWidth ./. pHeight * h
    ex = unit $ cross pUp ez
    ey = cross ez ex
    c  = (w *^ ex `plus` h *^ ey) /^ (-2) `minus` d
    x  = fCol /. pWidth * w
    y  = fRow /. pHeight * h

render
    :: Picture
    -> Object        -- ^ World
    -> Int           -- ^ Row
    -> Int           -- ^ Column
    -> Random Color
render picture world row col = hit world $ do
    dx <- sample (0, 1)
    dy <- sample (0, 1)
    shoot picture (row .+ dy) (col .+ dx)
  where
    (.+) = (+) . fromIntegral
