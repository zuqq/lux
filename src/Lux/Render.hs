{-# LANGUAGE RecordWildCards #-}

module Lux.Render
    ( Picture (..)
    , header
    , render
    , serialize
    ) where

import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)

import Lux.Color  (Color (..), white)
import Lux.Trace  (sample)
import Lux.Types  (Object, Ray (..))
import Lux.Vector ((*^), (/^), Vector (..), cross, len, minus, plus, unit)


-- | Newline-terminated PPM header.
header
    :: Int     -- ^ Width
    -> Int     -- ^ Height
    -> String
header w h = unlines ["P3", show w <> " " <> show h, "255"]

serialize :: Color -> String
serialize (Color r g b) = unwords $ show . floor . (255.99 *) <$> [r, g, b]

data Picture = Picture
    { pLens   :: !Vector  -- ^ Center of the lens.
    , pFocus  :: !Vector  -- ^ Center of the focal plane.
    , pUp     :: !Vector
    , pAngle  :: !Double  -- ^ Angle of view.
    , pWidth  :: !Int     -- ^ Width in pixels.
    , pHeight :: !Int     -- ^ Height in pixels.
    , pApert  :: !Double
    }

randPolar
    :: MonadRandom m
    => Double         -- ^ Radius of the closed disk to pick from.
    -> m Vector
randPolar maxRadius = do
    a <- getRandomR (0, 2 * pi)
    r <- getRandomR (0, maxRadius)
    return $ Vector (r * cos a) (r * sin a) 0

shoot
    :: MonadRandom m
    => Picture
    -> Double            -- ^ Row in fractional pixels
    -> Double            -- ^ Column in fractional pixels
    -> m Ray
shoot Picture {..} fRow fCol = do
    Vector dx dy _ <- randPolar (pApert / 2)
    let offset = dx *^ ex `plus` dy *^ ey
    return . Ray white (pLens `plus` offset) $
        (corner `plus` x *^ ex `plus` y *^ ey) `minus` offset
  where
    a /. b  = a / fromIntegral b
    a ./. b = fromIntegral a / fromIntegral b

    d      = pLens `minus` pFocus
    ez     = unit d
    z      = len d
    height = 2 * z * tan (pAngle / 2)
    width  = pWidth ./. pHeight * height
    ex     = unit $ cross pUp ez
    ey     = cross ez ex
    corner = (width *^ ex `plus` height *^ ey) /^ (-2) `minus` d
    x      = fCol /. pWidth * width
    y      = fRow /. pHeight * height

render
    :: MonadRandom m
    => Picture
    -> Object m        -- ^ World
    -> Int             -- ^ Row
    -> Int             -- ^ Column
    -> m Color
render picture world row col = sample world $ do
    dx <- getRandom
    dy <- getRandom
    shoot picture (row .+ dy) (col .+ dx)
  where
    (.+) = (+) . fromIntegral
