module Main
    ( main
    ) where

import System.Random (getStdGen)

import Lux.Color    (Color (..))
import Lux.Render   (Picture (..), fromList, fromPicture, render, withMaterial)
import Lux.Material (dielectric, diffuse, light, reflective)
import Lux.Sphere   (Sphere (..))
import Lux.Vector   (Vector (..))


main :: IO ()
main = do
    let world = fromList
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
        w = 800
        h = 400
        camera = fromPicture Picture
            { pLens   = Vector 4 2 4
            , pAngle  = pi / 4
            , pApert  = 0.1
            , pFocus  = Vector 0 1 0
            , pUp     = Vector 0 1 0
            , pWidth  = w
            , pHeight = h
            }

    putStrLn $ unwords ["P3", show w, show h, "255"]

    let serialize (Color r g b) = unwords $
            show . (truncate :: Double -> Int) . (255.999 *) <$> [r, g, b]

    let go (i, j) g
            | i < 0     = return ()
            | j == w    = go (i - 1, 0) g
            | otherwise = do
                let (c, g') = render world camera (i, j) g
                putStrLn . serialize $ c
                go (i, j + 1) g'

    g <- getStdGen
    go (h - 1, 0) g
