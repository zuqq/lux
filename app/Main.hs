{-# LANGUAGE RecordWildCards, ViewPatterns #-}

import Lux

main :: IO ()
main = do
    let width  = 400
        height = 400

    let world = fromList
            [ reifiedSphere (Vector (-1) 0.5 (-3)) 0.5 glacier specular
            , reifiedSphere (Vector 0 0.5 0) 0.5 glacier specular
            , reifiedSphere (Vector 0 (-100) 0) 100 green diffuse
            ]
        sky (unit -> Vector _ y _) = gradient white blue $ (y + 1) / 2
        camera = fromSpec CameraSpec
            { lens     = Vector 0 2 3
            , angle    = pi / 4
            , aperture = 0.25
            , focus    = Vector 0 1 0
            , up       = Vector 0 1 0
            , ..
            }
        scene = Scene {..}

    putStrLn $ unwords ["P3", show width, show height, "255"]

    let serialize (Color r g b) = unwords $
            show . (truncate :: Double -> Int) . (255.999 *) <$> [r, g, b]

    let go (i, j) x
            | i < 0      = pure ()
            | j == width = go (i - 1, 0) x
            | otherwise  = do
                let Result c y = run (render scene (i, j)) x
                putStrLn . serialize $ c
                go (i, j + 1) y

    go (height - 1, 0) 0
