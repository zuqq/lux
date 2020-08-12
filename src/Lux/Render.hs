{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Lux.Render
    ( Picture (..)
    , fromList
    , fromPicture
    , render
    , withMaterial
    ) where

import Data.Functor  ((<&>))
import Data.Foldable (foldl')

import System.Random (StdGen, uniformR)

import Lux.Color    ((*~), (~+~), Color (..), black, gradient, mix, navy, white)
import Lux.Material (Action (..), Material)
import Lux.Ray      (Ray (..), at)
import Lux.Sphere   (Sphere, normal, time)
import Lux.Vector   ((*^), Vector (..), cross, len, minus, plus, unit)


-- Picture ---------------------------------------------------------------------

data Picture = Picture
    { pLens   :: !Vector  -- ^ Center of the lens.
    , pAngle  :: !Double  -- ^ Angle of view.
    , pApert  :: !Double  -- ^ Aperture.
    , pFocus  :: !Vector  -- ^ Center of the focal plane.
    , pUp     :: !Vector  -- ^ "Up" direction.
    , pWidth  :: !Int     -- ^ Width in pixels.
    , pHeight :: !Int     -- ^ Height in pixels.
    }

-- Camera ----------------------------------------------------------------------

type Pixel  = (Int, Int)
type Camera = Pixel -> StdGen -> (Ray, StdGen)

-- | Offset for defocus blur.
uniformPolar :: Double -> StdGen -> ((Double, Double), StdGen)
uniformPolar maxRadius g =
    let (a, g')  = uniformR (0, 2 * pi) g
        (r, g'') = uniformR (0, maxRadius) g'
    in ((r * cos a, r * sin a), g'')

-- | Offset for antialiasing.
uniformCartesian :: StdGen -> ((Double, Double), StdGen)
uniformCartesian g =
    let (x, g')  = uniformR (0, 1) g
        (y, g'') = uniformR (0, 1) g'
    in ((x, y), g'')

fromPicture :: Picture -> Camera
fromPicture Picture {..} =
    let v = pLens `minus` pFocus
        h = 2 * len v * tan (pAngle / 2)  -- Height of the screen.
        s = h / fromIntegral pHeight      -- Side length of a pixel.
        w = fromIntegral pWidth * s       -- Width of the screen.
        z = unit v
        x = cross (unit pUp) z
        y = cross z x
        o = pFocus `minus` (w / 2) *^ x `minus` (h / 2) *^ y
    in \(i, j) g ->
        let ((dx, dy), g')  = uniformPolar (pApert / 2) g
            ((di, dj), g'') = uniformCartesian g'
            source = pLens `plus` dx *^ x `plus` dy *^ y
            target = o
                `plus` ((fromIntegral j + dj) * s) *^ x
                `plus` ((fromIntegral i + di) * s) *^ y
        in (Ray white source (target `minus` source), g'')

-- Object ----------------------------------------------------------------------

data Hit = Hit !Double !Action

instance Semigroup Hit where
    h @ (Hit t _) <> h' @ (Hit t' _) = if t <= t' then h else h'

type Object = Ray -> Maybe Hit

fromList :: [Object] -> Object
fromList objects ray = foldl' step Nothing objects
  where
    step mhit object = mhit <> object ray

withMaterial :: Material -> Sphere -> Object
withMaterial material sphere ray @ Ray {..} =
    time sphere ray <&> \t ->
        let p = ray `at` t
            n = normal sphere rDirection p
        in Hit t (material rColor rDirection p n)

-- Render ----------------------------------------------------------------------

type Sky = Vector -> Color

dusk :: Sky
dusk (unit -> Vector _ y _) = gradient white navy $ (y + 1) / 2

data Pair a b = Pair !a !b

bounce :: Object -> Ray -> StdGen -> (Color, StdGen)
bounce world ray g = go (50 :: Int) (Pair ray g)
  where
    go k (Pair ray' g') = if k <= 0
        then (rColor ray', g')
        else case world ray' of
            Nothing        -> (rColor ray' `mix` dusk (rDirection ray'), g')
            Just (Hit _ a) -> case a of
                Emit color -> (color, g')
                Scatter f  -> go (k - 1) $! uncurry Pair (f g')

average :: Int -> (StdGen -> (Color, StdGen)) -> StdGen -> (Color, StdGen)
average n f = go n . Pair black
  where
    go k (Pair c g) = if k <= 0
        then ((1 / fromIntegral n) *~ c, g)
        else go (k - 1) $! let (c', g') = f g in Pair (c ~+~ c') g'

render :: Object -> Camera -> Pixel -> StdGen -> (Color, StdGen)
render world camera pixel = average 100 $ \g ->
    let (ray, g') = camera pixel g
    in bounce world ray g'
