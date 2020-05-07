{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Lux.Trace
    ( sample
    ) where

import Lux.Color  ((/^), Color, black, plus)
import Lux.Types  (Hit (..), Object, Ray (..))
import Lux.Vector (len)


bounce :: Monad m => Object m -> m Ray -> m Color
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

sample :: Monad m => Object m -> m Ray -> m Color
sample world mray = go 100 $ return black
  where
    go k !acc = if k <= 0
        then (/^ 100) <$> acc
        else go (k - 1) $ plus <$> acc <*> bounce world mray
