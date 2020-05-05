{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( sample
    ) where

import Control.Monad              (replicateM)
import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)

import Lux.Color  ((*^), (/^), Color, black, blue, mix, plus, white)
import Lux.Types  (Hit (..), Object (..), Ray (..))
import Lux.Vector (Vector (..), unit)


-- | Linear white-to-blue gradient.
sky :: Vector -> Color
sky d = (1 - t) *^ white `plus` t *^ blue
  where
    t = let Vector _ y _ = unit d in (y + 1) / 2

randUnit :: MonadRandom m => m Vector
randUnit = do
    a <- getRandomR (0, 2 * pi)
    z <- getRandomR (-1, 1)
    let r = sqrt $ 1 - z * z
    return $ Vector (r * cos a) (r * sin a) z

bounce
    :: MonadRandom m
    => Object         -- ^ World
    -> m Ray
    -> m Color
bounce world = go 50
  where
    go k !acc = acc >>= \ray@Ray {..} -> if k <= 0
        then return $ mix rColor black
        else case hit world ray of
            Nothing        -> return $ mix rColor (sky rDirection)
            Just (Hit _ f) -> go (k - 1) (f <$> randUnit <*> getRandom)

sample
    :: MonadRandom m
    => Object         -- ^ World
    -> m Ray
    -> m Color
sample world mray = go 100 $ return black
  where
    go k !acc = if k <= 0
        then (/^ 100) <$> acc
        else go (k - 1) $ plus <$> acc <*> bounce world mray
