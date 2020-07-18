{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( hit
    ) where

import Lux.Color   ((*^), (/^), Color, black, navy, mix, plus, white)
import Lux.Random  (Random)
import Lux.Types   (Action (..), Hit (..), Object, Ray (..))
import Lux.Vector  (Vector (Vector), unit)


-- | Linear white-to-navy gradient.
sky :: Vector -> Color
sky d = (1 - t) *^ white `plus` t *^ navy
  where
    t = let Vector _ y _ = unit d in (y + 1) / 2

bounce :: Object -> Random Ray -> Random Color
bounce world = go (50 :: Int)
  where
    go k !acc = if k <= 0
        then rColor <$> acc
        else acc >>= \ray @ Ray {..} -> case world ray of
            Nothing        -> return $ mix rColor (sky rDirection)
            Just (Hit _ a) -> case a of
                Emit color   -> return color
                Scatter mray -> go (k - 1) mray

hit :: Object -> Random Ray -> Random Color
hit world mray = go (100 :: Int) (return black)
  where
    go k !acc = if k <= 0
        then (/^ 100) <$> acc
        else go (k - 1) $ plus <$> acc <*> bounce world mray
