module Main
    ( main
    ) where

import Data.Foldable (for_)
import Data.IORef    (newIORef, readIORef, writeIORef)
import System.IO     (hPutStrLn, stderr)

import System.Random (getStdGen)

import Lux.Color    (Color (..), gray, white)
import Lux.Render   (Picture (..), fromList, fromPicture, render, withMaterial)
import Lux.Material (dielectric, diffuse, light, reflective)
import Lux.Sphere   (Sphere (..))
import Lux.Vector   (Vector (..))


-- | Plain PPM header, see <http://netpbm.sourceforge.net/doc/ppm.html>.
header
    :: Int     -- ^ Width
    -> Int     -- ^ Height
    -> String
header w h = unwords ["P3", show w, show h, "255"]

serialize :: Color -> String
serialize (Color r g b) = unwords $
    show . (floor :: Double -> Int) . (255.999 *) <$> [r, g, b]

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
        picture = Picture
            { pLens   = Vector 4 2 4
            , pFocus  = Vector 0 1 0
            , pUp     = Vector 0 1 0
            , pAngle  = pi / 4
            , pWidth  = w
            , pHeight = h
            , pApert  = 0.1
            }
        camera = fromPicture picture

    putStrLn $ header w h
    g <- getStdGen
    v <- newIORef g
    for_ [h - 1, h - 2..0] $ \i -> do
        hPutStrLn stderr $ "On row " <> show i
        for_ [0..w - 1] $ \j -> do
            (color, g') <- render world camera (i, j) <$> readIORef v
            putStrLn . serialize $ color
            writeIORef v g'
