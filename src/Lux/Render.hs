{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Render
    ( Picture (..)
    , Scene (..)
    , fromList
    , fromPicture
    , render
    , withMaterial
    ) where

import Data.Functor  ((<&>))
import Data.Foldable (foldl')

import System.Random (StdGen, uniformR)

import Lux.Color    ((*~), (~+~), Color (..), black, mix, white)
import Lux.Material (Material)
import Lux.Ray      (Ray (..), at)
import Lux.Sphere   (Sphere, normal, time)
import Lux.Vector   ((*^), Vector (..), cross, len, minus, plus, unit)


-- Picture ---------------------------------------------------------------------

data Picture = Picture
    { lens   :: !Vector  -- ^ Center of the lens.
    , angle  :: !Double  -- ^ Angle of view.
    , apert  :: !Double  -- ^ Aperture.
    , focus  :: !Vector  -- ^ Center of the focal plane.
    , up     :: !Vector  -- ^ "Up" direction.
    , width  :: !Int     -- ^ Width in pixels.
    , height :: !Int     -- ^ Height in pixels.
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
    let v = lens `minus` focus
        h = 2 * len v * tan (angle / 2)  -- Height of the screen.
        s = h / fromIntegral height      -- Side length of a pixel.
        w = fromIntegral width * s       -- Width of the screen.
        z = unit v
        x = cross (unit up) z
        y = cross z x
        o = focus `minus` (w / 2) *^ x `minus` (h / 2) *^ y
    in \(i, j) g ->
        let ((dx, dy), g')  = uniformPolar (apert / 2) g
            ((di, dj), g'') = uniformCartesian g'
            source = lens `plus` dx *^ x `plus` dy *^ y
            target = o
                `plus` ((fromIntegral j + dj) * s) *^ x
                `plus` ((fromIntegral i + di) * s) *^ y
        in (Ray source (target `minus` source), g'')

-- Object ----------------------------------------------------------------------

data Hit = Hit !Double !(StdGen -> ((Color, Ray), StdGen))

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
            n = normal sphere direction p
        in Hit t (material direction p n)

-- Tracing ---------------------------------------------------------------------

type Sky = Vector -> Color

trace :: Object -> Sky -> Ray -> StdGen -> (Color, StdGen)
trace world sky = go (50 :: Int) white
  where
    go k !color ray g = if k <= 0
        then (color, g)
        else case world ray of
            Nothing        -> (color `mix` sky (direction ray), g)
            Just (Hit _ f) ->
                let ((color', ray'), g') = f g
                in go (k - 1) (color `mix` color') ray' g'

average :: Int -> (StdGen -> (Color, StdGen)) -> StdGen -> (Color, StdGen)
average n f = go n black
  where
    go k !c g = if k <= 0
        then ((1 / fromIntegral n) *~ c, g)
        else let (c', g') = f g in go (k - 1) (c ~+~ c') g'

-- Scene -----------------------------------------------------------------------

data Scene = Scene
    { world  :: Object
    , sky    :: Sky
    , camera :: Camera
    }

render :: Scene -> Pixel -> StdGen -> (Color, StdGen)
render Scene {..} pixel = average 100 $ \g ->
    let (ray, g') = camera pixel g
    in trace world sky ray g'
