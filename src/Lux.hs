{-# LANGUAGE BangPatterns, RecordWildCards #-}

-- | A purely functional implementation of <https://raytracing.github.io/books/RayTracingInOneWeekend.html Ray Tracing in One Weekend>.
--
-- = Usage
--
-- Construct a 'Scene' and 'render' away.
module Lux
    (
    -- * Camera specification
      CameraSpec (..)
    -- * Camera
    , Pixel
    , Camera
    , fromSpec
    -- * Objects
    , Hit (..)
    , Object
    , fromList
    -- * Materials
    , Material
    , diffuse
    , specular
    -- * Constructors
    , sphere
    -- * Rendering
    , Sky
    , Scene (..)
    , render
    -- * Reexports
    , module Lux.Color
    , module Lux.Random
    , module Lux.Vector
    )
    where

import Control.Monad ((<=<))
import Data.Foldable (foldl')
import Data.Functor  ((<&>))

import Lux.Ray    (Ray (..), at)
import Lux.Sphere (Sphere (..), normal, time)

import Lux.Color
import Lux.Random
import Lux.Vector

-- | Camera specification.
--
-- The fields have the following meanings:
--
--     * 'lens', 'focus', and 'up' determine the image plane;
--
--     * 'angle' determines the viewport;
--
--     * 'aperture' controls depth of field;
--
--     * 'width' and 'height' determine the viewport's segmentation into pixels.
data CameraSpec = CameraSpec
    { lens     :: Vector  -- ^ Center of the lens.
    , angle    :: Double  -- ^ Angle of view.
    , aperture :: Double  -- ^ Diameter of the lens.
    , focus    :: Vector  -- ^ Center of the focal plane.
    , up       :: Vector  -- ^ Upward direction.
    , width    :: Int     -- ^ Width in pixels.
    , height   :: Int     -- ^ Height in pixels.
    }

-- | Every pixel in the image is uniquely identified by the two non-negative
-- integers that are its indices in the two-dimensional array representing the
-- picture.
type Pixel = (Int, Int)

-- | A 'Camera' aims a 'Ray' at a given 'Pixel'.
--
-- Returning a 'Random' ray and sampling multiple times allows for depth of
-- field and anti-aliasing.
type Camera = Pixel -> Random Ray

-- | Construct a 'Camera' from a 'CameraSpec'.
--
-- The ray generated by this camera is random in two ways:
--
--     * The source of the ray is drawn at random from a disk of diameter
--       'aperture' around the center of the lens in order to simulate depth of
--       field.
--
--     * The target of the ray is drawn at random from the pixel that we are
--       aiming at, for anti-aliasing purposes.
fromSpec :: CameraSpec -> Camera
fromSpec CameraSpec {..} =
    let v = lens `minus` focus
        h = 2 * len v * tan (angle / 2)  -- Height of the screen.
        s = h / fromIntegral height      -- Side length of a pixel.
        w = fromIntegral width * s       -- Width of the screen.
        z = unit v
        x = cross (unit up) z
        y = cross z x
        o = focus `minus` (w / 2) *^ x `minus` (h / 2) *^ y
    in \(i, j) -> do
        (dx, dy) <- sampleDisk (aperture / 2)
        (di, dj) <- sampleUnitSquare
        let source = lens `plus` dx *^ x `plus` dy *^ y
            target = o
                `plus` ((fromIntegral j + dj) * s) *^ x
                `plus` ((fromIntegral i + di) * s) *^ y
        pure $ Ray source (target `minus` source)

-- | The product of a collision between a 'Ray' and an 'Object'.
--
-- NB. The idea behind making only the first field strict is that this field
-- always needs to be evaluated, whereas the other fields only need to be
-- evaluated for the closest hit.
data Hit = Hit
    {-# UNPACK #-} !Double  -- ^ Time.
    Color                   -- ^ Attenuation.
    (Random Ray)            -- ^ Scattered ray.

-- | Lifts the @'Data.Semigroup.Min' Double@ semigroup instance.
instance Semigroup Hit where
    h@(Hit t _ _) <> h'@(Hit t' _ _) = if t <= t' then h else h'

-- | An 'Object' is anything that can be probed with a 'Ray'.
type Object = Ray -> Maybe Hit

-- | Probing @fromList objects@ with a 'Ray' returns the closest 'Hit'.
fromList :: [Object] -> Object
fromList objects ray = foldl' step Nothing objects
  where
    step mhit object = mhit <> object ray

type Material
    =  Vector  -- ^ Point of impact.
    -> Vector  -- ^ Direction of the incoming ray.
    -> Vector  -- ^ Unit normal at the point of impact.
    -> Random Ray

-- | Diffuse 'Material' that randomly scatters the incoming 'Ray'.
diffuse :: Material
diffuse p _ n = sampleUnitSphere <&> \u -> Ray p (n `plus` u)

-- | Specular 'Material' that reflects the incoming 'Ray'.
specular :: Material
specular p v n = pure $ Ray p (reflect v n)

-- | Construct a reified 'Sphere'.
sphere
    :: Vector  -- ^ Center.
    -> Double  -- ^ Radius.
    -> Color
    -> Material
    -> Object
sphere center radius color material ray =
    let sphere_ = Sphere {..}
    in time sphere_ ray <&> \t ->
        let p = ray `at` t
        in Hit t color (material p (direction ray) (normal sphere_ p))

type Sky = Vector -> Color

-- | Trace a 'Ray'.
--
-- Recursively traces the 'Ray' until it misses the 'Object' or the maximal
-- recursion depth is reached.
trace :: Object -> Sky -> Ray -> Random Color
trace world sky = go (50 :: Int) white
  where
    go k !c !r = if k <= 0
        then pure c
        else case world r of
            Nothing           -> pure $ c `mix` sky (direction r)
            Just (Hit _ a mr) -> mr >>= go (k - 1) (a `mix` c)

average :: Int -> Random Color -> Random Color
average n mc = go n black
  where
    go k !c = if k <= 0
        then pure $ (1 / fromIntegral n) *~ c
        else mc >>= go (k - 1) . (c ~+~)

-- | Image specification.
data Scene = Scene
    { world  :: Object
    , sky    :: Sky
    , camera :: Camera
    }

-- | Render a 'Pixel'.
render :: Scene -> Pixel -> Random Color
render Scene {..} = average 100 . (trace world sky <=< camera)
