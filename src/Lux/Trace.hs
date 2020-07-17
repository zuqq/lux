{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( sample
    ) where

import Lux.Color   ((*^), (/^), Color, black, navy, mix, plus, white)
import Lux.Scatter (scatter)
import Lux.Types   (Object (..), Ray (..))
import Lux.Vector  (Vector (Vector), unit)


-- | Linear white-to-navy gradient.
sky :: Vector -> Color
sky d = (1 - t) *^ white `plus` t *^ navy
  where
    t = let Vector _ y _ = unit d in (y + 1) / 2

bounce :: Object -> IO Ray -> IO Color
bounce world = go (50 :: Int)
  where
    go k !acc = if k <= 0
        then rColor <$> acc
        else acc >>= \ray @ Ray {..} -> case hit world ray of
            Nothing -> return $ mix rColor (sky rDirection)
            Just h  -> case scatter ray h of
                Left color -> return color
                Right mray -> go (k - 1) mray

sample :: Object -> IO Ray -> IO Color
sample world mray = go (100 :: Int) $ return black
  where
    go k !acc = if k <= 0
        then (/^ 100) <$> acc
        else go (k - 1) $ plus <$> acc <*> bounce world mray
