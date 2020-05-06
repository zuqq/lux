{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( sample
    ) where

import Control.Monad.Random.Class (MonadRandom)

import Lux.Color  ((*^), (/^), Color, black, blue, mix, plus, white)
import Lux.Types  (Hit (..), Object, Ray (..))
import Lux.Vector (Vector (..), len, unit)


-- | Linear white-to-blue gradient.
sky :: Vector -> Color
sky d = (1 - t) *^ white `plus` t *^ blue
  where
    t = let Vector _ y _ = unit d in (y + 1) / 2

bounce
    :: MonadRandom m
    => Object m       -- ^ World
    -> m Ray
    -> m Color
bounce world = go 50
  where
    go k !acc = if k <= 0
        then rColor <$> acc
        else acc >>= \ray -> case world ray of
            Nothing           -> return black
            Just (Hit _ mray) -> mray >>= \Ray {..} ->
                if len rDirection > 0
                    then go (k - 1) mray
                    else return rColor

sample
    :: MonadRandom m
    => Object m       -- ^ World
    -> m Ray
    -> m Color
sample world mray = go 100 $ return black
  where
    go k !acc = if k <= 0
        then (/^ 100) <$> acc
        else go (k - 1) $ plus <$> acc <*> bounce world mray
