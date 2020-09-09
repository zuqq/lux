{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Main
    ( main
    ) where

import System.Random (getStdGen)

import Lux.Color    (Color (..), blue, glacier, gradient, green, white)
import Lux.Render
import Lux.Material (dielectric, diffuse, reflective)
import Lux.Sphere   (Sphere (..))
import Lux.Vector   (Vector (..), unit)


main :: IO ()
main = do
    let world = fromList
            [ withMaterial (reflective glacier)
                (Sphere (Vector (-1) 0.5 (-3)) 0.5)
            , withMaterial (reflective glacier)
                (Sphere (Vector 0 0.5 0) 0.5)
            , withMaterial (diffuse green)
                (Sphere (Vector 0 (-100) 0) 100)
            ]
        sky (unit -> Vector _ y _) = gradient white blue $ (y + 1) / 2
        width  = 400
        height = 400
        camera = fromPicture Picture
            { lens   = Vector 0 2 3
            , angle  = pi / 4
            , apert  = 0.25
            , focus  = Vector 0 1 0
            , up     = Vector 0 1 0
            , width  = width
            , height = height
            }
        scene = Scene {..}

    putStrLn $ unwords ["P3", show width, show height, "255"]

    let serialize (Color r g b) = unwords $
            show . (truncate :: Double -> Int) . (255.999 *) <$> [r, g, b]

    let go (i, j) g
            | i < 0      = return ()
            | j == width = go (i - 1, 0) g
            | otherwise  = do
                let (c, g') = render scene (i, j) g
                putStrLn . serialize $ c
                go (i, j + 1) g'

    g <- getStdGen
    go (height - 1, 0) g
