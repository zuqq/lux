{-# LANGUAGE BangPatterns, RecordWildCards #-}

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

data CameraSpec = CameraSpec
    { lens     :: Vector  -- ^ Center of the lens.
    , angle    :: Double  -- ^ Angle of view.
    , aperture :: Double
    , focus    :: Vector  -- ^ Center of the focal plane.
    , up       :: Vector  -- ^ Upward direction.
    , width    :: Int     -- ^ Width in pixels.
    , height   :: Int     -- ^ Height in pixels.
    }

type Pixel = (Int, Int)

{-
    Using existentials, we could get more general types like

    > type Camera = forall m. MonadRandom m => Pixel -> m Ray

    instead of the hard-coded 'Random' monad. I decided against it because that
    seems like an awful lot of complexity for relatively little gain.
-}
type Camera = Pixel -> Random Ray

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
        {-
            There's randomness in two places:

            * The source of the ray is drawn at random from a disk of diameter
              'aperture' around the center of the lens in order to simulate
              depth of field.

            * The target of the ray is drawn at random from the pixel that we
              are aiming at, for anti-aliasing purposes.
        -}
        (dx, dy) <- sampleDisk (aperture / 2)
        (di, dj) <- sampleUnitSquare
        let source = lens `plus` dx *^ x `plus` dy *^ y
            target = o
                `plus` ((fromIntegral j + dj) * s) *^ x
                `plus` ((fromIntegral i + di) * s) *^ y
        pure $ Ray source (target `minus` source)

data Hit = Hit {-# UNPACK #-} !Double Color (Random Ray)

instance Semigroup Hit where
    h@(Hit t _ _) <> h'@(Hit t' _ _) = if t <= t' then h else h'

type Object = Ray -> Maybe Hit

fromList :: [Object] -> Object
fromList objects ray = foldl' step Nothing objects
  where
    step mhit object = mhit <> object ray

type Material
    =  Vector  -- ^ Point of impact.
    -> Vector  -- ^ Direction of the incoming ray.
    -> Vector  -- ^ Unit normal at the point of impact.
    -> Random Ray

diffuse :: Material
diffuse p _ n = sampleUnitSphere <&> \u -> Ray p (n `plus` u)

specular :: Material
specular p v n = pure $ Ray p (reflect v n)

-- | Smart constructor for reified 'Sphere's.
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

data Scene = Scene
    { world  :: Object
    , sky    :: Sky
    , camera :: Camera
    }

{-
    Note that

    > average 100 . trace world sky <=< camera

    would be wrong here. Indeed, since '(.)' binds more tightly than '(<=<)',
    that's equivalent to

    > (average 100 . trace world sky) <=< camera

    which doesn't average over 'camera'.
-}
render :: Scene -> Pixel -> Random Color
render Scene {..} = average 100 . (trace world sky <=< camera)
