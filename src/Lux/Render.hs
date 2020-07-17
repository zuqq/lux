{-# LANGUAGE RecordWildCards #-}

module Lux.Render
    ( Picture (..)
    , header
    , render
    , serialize
    ) where

import System.Random (randomIO, randomRIO)

import Lux.Color  (Color (..), white)
import Lux.Trace  (sample)
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
    , pUp     :: !Vector
    , pAngle  :: !Double  -- ^ Angle of view.
    , pWidth  :: !Int     -- ^ Width in pixels.
    , pHeight :: !Int     -- ^ Height in pixels.
    , pApert  :: !Double
    }

randomPolar
    :: Double              -- ^ Radius of the closed disk to pick from.
    -> IO (Double, Double)
randomPolar maxRadius = do
    a <- randomRIO (0, 2 * pi)
    r <- randomRIO (0, maxRadius)
    return (r * cos a, r * sin a)

shoot
    :: Picture
    -> Double            -- ^ Row in fractional pixels.
    -> Double            -- ^ Column in fractional pixels.
    -> IO Ray
shoot Picture {..} fRow fCol = do
    (dx, dy) <- randomPolar (pApert / 2)
    let offset = dx *^ ex `plus` dy *^ ey
    return . Ray white (pLens `plus` offset) $
        (c `plus` x *^ ex `plus` y *^ ey) `minus` offset
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
    -> Object          -- ^ World
    -> Int             -- ^ Row
    -> Int             -- ^ Column
    -> IO Color
render picture world row col = sample world $ do
    dx <- randomIO
    dy <- randomIO
    shoot picture (row .+ dy) (col .+ dx)
  where
    (.+) = (+) . fromIntegral
