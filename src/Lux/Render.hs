{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Lux.Render
    ( Frame
    , Hit (..)
    , Object
    , Picture (..)
    , Pixel
    , Scene (..)
    , Sky
    , fromList
    , fromPicture
    , render
    )
    where

import Control.Monad ((<=<))
import Data.Foldable (foldl')

import Lux.Color  ((*~), (~+~), Color (..), black, mix, white)
import Lux.Random (Random, sampleDisk, sampleUnitSquare)
import Lux.Ray    (Ray (..))
import Lux.Vector ((*^), Vector (..), cross, len, minus, plus, unit)

data Picture = Picture
    { lens     :: Vector  -- ^ Center of the lens.
    , angle    :: Double  -- ^ Angle of view.
    , aperture :: Double
    , focus    :: Vector  -- ^ Center of the focal plane.
    , up       :: Vector  -- ^ "Up" direction.
    , width    :: Int     -- ^ Width in pixels.
    , height   :: Int     -- ^ Height in pixels.
    }

type Pixel = (Int, Int)

{-
    Using existentials, we could get more general types like

    > type Frame = forall m. MonadRandom m => Pixel -> m Ray

    instead of the hard-coded 'Random' monad. I decided against it because that
    seems like an awful lot of complexity for relatively little gain.
-}
type Frame = Pixel -> Random Ray

fromPicture :: Picture -> Frame
fromPicture Picture {..} =
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
    { world :: Object
    , sky   :: Sky
    , frame :: Frame
    }

{-
    Note that

    > average 100 . trace world sky <=< frame

    would be wrong here. Indeed, since '(.)' binds more tightly than '(<=<)',
    that's equivalent to

    > (average 100 . trace world sky) <=< frame

    which doesn't average over 'frame'.
-}
render :: Scene -> Pixel -> Random Color
render Scene {..} = average 100 . (trace world sky <=< frame)
