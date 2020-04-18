{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (for_)
import Data.List     (intercalate)
import System.IO     (hPutStrLn, stderr)

import Lux.Render
import Lux.Vector


render :: Scene -> IO ()
render scene@Scene {..} = do
    -- PPM header
    putStrLn $ "P3\n" <> show sWidth <> " " <> show sHeight <> "\n255"
    for_ [sHeight - 1, sHeight - 2..0] $ \j -> do
        -- Progress report
        hPutStrLn stderr $ "On row " <> show j
        for_ [0..sWidth - 1] $ \i -> do
            Vector r g b <- withAA scene i j
            putStrLn . intercalate " " $
                show . floor . (255.99 *) <$> [r, g, b]

main :: IO ()
main = render Scene
    { sWidth      = 800
    , sHeight     = 400
    , sEye        = Vector 0 0 0
    , sLowerLeft  = Vector (-2) (-1) (-1)
    , sHorizontal = Vector 4 0 0 
    , sVertical   = Vector 0 2 0
    , sWorld      = fromList
        [ mkSphere (Vector 0 0 (-1)) 0.5
        , mkSphere (Vector 0 (-100.5) (-1)) 100
        ]
    }
