{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( sample
    ) where

import Control.Monad              (replicateM)
import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)

import Lux.Color  (Color, average, black, mix, sky)
import Lux.Types  (Hit (..), Object (..), Ray (..))
import Lux.Vector (Vector (..))


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
sample world mray = average <$> replicateM 100 (bounce world mray)
