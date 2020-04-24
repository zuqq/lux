module Main where

import Control.Monad ((>=>))
import Data.Foldable (traverse_)

import Lux.Picture
import Lux.Render
import Lux.Vector


main :: IO ()
main = putStr (header w h) >>
    traverse_ (render picture world >=> putStrLn . serialize)
        [ (col, row) | row <- [h - 1, h - 2..0], col <- [0..w - 1] ]
  where
    w = 800
    h = 400
    picture = Picture
        { pCenter = Vector 0 1 1
        , pFocus  = Vector 0 0 (-1)
        , pUp     = Vector 0 1 0
        , pAngle  = pi / 4
        , pWidth  = w
        , pHeight = h
        }
    world = fromList
        [ metalSphere (Sphere (Vector (-0.5) 0 (-1)) 0.5) (Vector 0.8 0.6 0.2)
        , metalSphere (Sphere (Vector 0.5 0 (-1)) 0.5) (Vector 0.8 0.8 0.8)
        , lambSphere (Sphere (Vector 0 (-100.5) (-1)) 100) (Vector 0.8 0.8 0)
        ]
