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
        w = 400
        h = 400
        camera = fromPicture Picture
            { pLens   = Vector 0 2 3
            , pAngle  = pi / 4
            , pApert  = 0.25
            , pFocus  = Vector 0 1 0
            , pUp     = Vector 0 1 0
            , pWidth  = w
            , pHeight = h
            }
        scene = Scene {..}

    putStrLn $ unwords ["P3", show w, show h, "255"]

    let serialize (Color r g b) = unwords $
            show . (truncate :: Double -> Int) . (255.999 *) <$> [r, g, b]


    let go (i, j) g
            | i < 0     = return ()
            | j == w    = go (i - 1, 0) g
            | otherwise = do
                let (c, g') = render scene (i, j) g
                putStrLn . serialize $ c
                go (i, j + 1) g'

    g <- getStdGen
    go (h - 1, 0) g
