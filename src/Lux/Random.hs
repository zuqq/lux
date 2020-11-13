{-# LANGUAGE DeriveFunctor #-}

module Lux.Random
    ( Random (..)
    , Result (..)
    , sampleDisk
    , sampleInterval
    , sampleUnitSphere
    , sampleUnitSquare
    ) where

import Data.Bits (unsafeShiftR, xor)
import Data.Word (Word64)

import Lux.Vector (Vector (Vector))


data Result a = Result !a {-# UNPACK #-} !Word64
    deriving Functor

newtype Random a = Random {run :: Word64 -> Result a}
    deriving Functor

instance Applicative Random where
    pure a  = Random $ \x -> Result a x
    u <*> r = Random $ \x -> let Result f y = run u x in run (fmap f r) y

instance Monad Random where
    r >>= c = Random $ \x -> let Result a y = run r x in run (c a) y

next :: Random Double
next = Random $ \x ->
  let y = x + 0x9e3779b97f4a7c15
      a = (y `xor` (y `unsafeShiftR` 30)) * 0xbf58476d1ce4e5b9
      b = (a `xor` (a `unsafeShiftR` 27)) * 0x94d049bb133111eb
      c =  b `xor` (b `unsafeShiftR` 31)
      d = fromIntegral (c `unsafeShiftR` 11) / 0x0020000000000000
  in Result d y

sampleDisk :: Double -> Random (Double, Double)
sampleDisk radius = do
    a <- sampleInterval (0, 2 * pi)
    r <- sampleInterval (0, radius)
    pure (r * cos a, r * sin a)

sampleInterval :: (Double, Double) -> Random Double
sampleInterval (a, b) = do
    x <- next
    pure $ a + (b - a) * x

sampleUnitSquare :: Random (Double, Double)
sampleUnitSquare = do
    x <- next
    y <- next
    pure (x, y)

sampleUnitSphere :: Random Vector
sampleUnitSphere = do
    a <- sampleInterval (0, 2 * pi)
    z <- sampleInterval (-1, 1)
    let r = sqrt $ 1 - z * z
    pure $ Vector (r * cos a) (r * sin a) z
