{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad ((>=>))
import Data.Foldable (for_)
import System.IO     (hPutStrLn, stderr)

import Lux.Color  (Color (..))
import Lux.Render (Picture (..), header, render, serialize)
import Lux.Sphere (Sphere (..), glassSphere, lambSphere, metalSphere)
import Lux.Types  (fromList)
import Lux.Vector (Vector (..))


main :: IO ()
main = do
    let Picture {..} = picture
    putStr (header pWidth pHeight)
    for_ [pHeight - 1, pHeight - 2..0] $ \row -> do
        hPutStrLn stderr $ "On row " <> show row
        for_ [0..pWidth - 1] $
            render picture world row >=> serialize >>> putStrLn
  where
    (>>>) = flip (.)

    picture = Picture
        { pLens   = Vector 4 2 4
        , pFocus  = Vector 0 1 0
        , pUp     = Vector 0 1 0
        , pAngle  = pi / 4
        , pWidth  = 800
        , pHeight = 400
        , pApert  = 0.1
        }
    world = fromList
        [ lambSphere (Sphere (Vector 0 (-1000) 0) 1000) (Color 0.5 0.5 0.5)
        , lambSphere (Sphere (Vector 0 1 (-2)) 1) (Color 0.4 0.2 0.1)
        , glassSphere (Sphere (Vector 0 1 0) 1) 1.5
        , metalSphere (Sphere (Vector 0 1 2) 1) (Color 0.7 0.6 0.5)
        ]
