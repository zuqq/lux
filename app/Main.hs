{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import Control.Monad.Random.Strict (evalRandIO)
import Data.Foldable               (for_)
import System.IO                   (hPutStrLn, stderr)

import Lux.Color    (Color (..))
import Lux.Render   (Picture (..), header, render, serialize)
import Lux.Material (dielectric, diffuse, light, reflective)
import Lux.Sphere   (Sphere (..), withMaterial)
import Lux.Types    (fromList)
import Lux.Vector   (Vector (..))


main :: IO ()
main = do
    let Picture {..} = picture
    putStrLn $ header pWidth pHeight
    for_ [pHeight - 1, pHeight - 2..0] $ \row -> do
        hPutStrLn stderr $ "On row " <> show row
        for_ [0..pWidth - 1] $ \col -> do
            color <- evalRandIO $ render picture world row col
            putStrLn . serialize $ color
  where
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
        [ withMaterial (diffuse (Color 0.5 0.5 0.5))
                       (Sphere (Vector 0 (-1000) 0) 1000)
        , withMaterial (diffuse (Color 0.4 0.2 0.1))
                       (Sphere (Vector 0 1 (-2)) 1)
        , withMaterial (dielectric 1.5)
                       (Sphere (Vector 0 1 0) 1)
        , withMaterial (reflective (Color 0.7 0.6 0.5))
                       (Sphere (Vector 0 1 2) 1)
        , withMaterial (light (Color 1 1 1))
                       (Sphere (Vector 0 5 2) 1)
        , withMaterial (light (Color 1 1 1))
                       (Sphere (Vector (-5) 5 2) 1)
        , withMaterial (light (Color 1 1 1))
                       (Sphere (Vector 5 5 2) 1)
        ]
