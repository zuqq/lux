{-# LANGUAGE RecordWildCards #-}

module Lux.Picture where

import Control.Monad.Random.Class (MonadRandom, getRandom)

import Lux.Render
import Lux.Vector


-- | Newline-terminated PPM header.
header
    :: Int     -- ^ Width
    -> Int     -- ^ Height
    -> String
header w h = unlines ["P3", show w <> " " <> show h, "255"]

serialize :: Vector -> String
serialize (Vector r g b) = unwords $ show . floor . (255.99 *) <$> [r, g, b]

data Picture = Picture
    { pCenter :: !Vector
    , pFocus  :: !Vector
    , pUp     :: !Vector
    , pAngle  :: !Double
    , pWidth  :: !Int
    , pHeight :: !Int
    }

shoot
    :: Picture
    -> (Double, Double)  -- Coordinates of the point in fractional pixels.
    -> Ray
shoot Picture {..} (fCol, fRow) = Ray white (pCenter `plus` ez) $
    origin `plus` x *^ ex `plus` y *^ ey
  where
    a /. b  = a / fromIntegral b
    a ./. b = fromIntegral a / fromIntegral b

    height = 2 * tan (pAngle / 2)
    width  = pWidth ./. pHeight * height
    ez     = unit (pCenter `minus` pFocus)
    ex     = unit (cross pUp ez)
    ey     = cross ez ex
    origin = (width *^ ex `plus` height *^ ey) /^ (-2) `minus` ez
    x      = fCol /. pWidth * width
    y      = fRow /. pHeight * height

render
    :: MonadRandom m
    => Picture
    -> Object          -- ^ World
    -> (Int, Int)      -- ^ (Column, Row)
    -> m Vector
render picture world (col, row) = sample world $ do
    dx <- getRandom
    dy <- getRandom
    return $ shoot picture (col .+ dx, row .+ dy)
  where
    (.+) = (+) . fromIntegral
