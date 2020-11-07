{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Main
    ( main
    ) where

import Lux.Color    (Color (..), blue, glacier, gradient, green, white)
import Lux.Material (diffuse, specular, sphere)
import Lux.Random   (Result (..), run)
import Lux.Render   (Picture (..), Scene (..), fromList, fromPicture, render)
import Lux.Vector   (Vector (..), unit)


main :: IO ()
main = do
    let world = fromList
            [ sphere (Vector (-1) 0.5 (-3)) 0.5 glacier specular
            , sphere (Vector 0 0.5 0) 0.5 glacier specular
            , sphere (Vector 0 (-100) 0) 100 green diffuse
            ]
        sky (unit -> Vector _ y _) = gradient white blue $ (y + 1) / 2
        width  = 400
        height = 400
        frame = fromPicture Picture
            { lens     = Vector 0 2 3
            , angle    = pi / 4
            , aperture = 0.25
            , focus    = Vector 0 1 0
            , up       = Vector 0 1 0
            , width    = width
            , height   = height
            }
        scene = Scene {..}

    putStrLn $ unwords ["P3", show width, show height, "255"]

    let serialize (Color r g b) = unwords $
            show . (truncate :: Double -> Int) . (255.999 *) <$> [r, g, b]

    let go (i, j) x
            | i < 0      = return ()
            | j == width = go (i - 1, 0) x
            | otherwise  = do
                let Result c y = run (render scene (i, j)) x
                putStrLn . serialize $ c
                go (i, j + 1) y

    go (height - 1, 0) 0
