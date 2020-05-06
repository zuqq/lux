{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( sample
    ) where

import Control.Monad.Random.Class (MonadRandom)

import Lux.Color  ((*^), (/^), Color, black, blue, mix, plus, white)
import Lux.Types  (Hit (..), Object, Ray (..))
import Lux.Vector (Vector (..), unit)


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
    go k !acc = acc >>= \ray@Ray {..} -> if k <= 0
        then return $ mix rColor black
        else case world ray of
            Nothing           -> return $ mix rColor (sky rDirection)
            Just (Hit _ mray) -> go (k - 1) mray

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
