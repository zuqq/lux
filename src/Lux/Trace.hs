{-# LANGUAGE BangPatterns #-}

module Lux.Trace
    ( sample
    ) where

import Control.Monad.Random.Class (MonadRandom)

import Lux.Color   ((/^), Color, black, plus)
import Lux.Scatter (scatter)
import Lux.Types   (Object (..), Ray (..))


bounce :: MonadRandom m => Object -> m Ray -> m Color
bounce world = go (50 :: Int)
  where
    go k !acc = if k <= 0
        then rColor <$> acc
        else acc >>= \ray -> case hit world ray of
            Nothing -> return black
            Just h  -> case scatter ray h of
                Left color -> return color
                Right mray -> go (k - 1) mray

sample :: MonadRandom m => Object -> m Ray -> m Color
sample world mray = go (100 :: Int) $ return black
  where
    go k !acc = if k <= 0
        then (/^ 100) <$> acc
        else go (k - 1) $ plus <$> acc <*> bounce world mray
