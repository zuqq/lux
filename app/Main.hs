module Main where

import Control.Monad              ((>=>))
import Control.Monad.Random.Class (MonadRandom, getRandomR)
import Data.Foldable              (traverse_)
import Data.List                  (intercalate)

import Lux.Render
import Lux.Vector


-- | PPM header.
header
    :: Int     -- ^ Width
    -> Int     -- ^ Height
    -> String
header w h = intercalate "\n" ["P3", show w <> " " <> show h, "255"]

serialize :: Vector -> String
serialize (Vector r g b) = intercalate " " $
    show . floor . (255.99 *) <$> [r, g, b]

render
    :: MonadRandom m
    => Object          -- ^ World
    -> (Int, Int)      -- ^ (Column, Row)
    -> m Vector
render world (col, row) = sample world mray
  where
    (.+) = (+) . fromIntegral
    mray = do
        dx <- getRandomR (0, 1)
        dy <- getRandomR (0, 1)
        let x = (col .+ dx) / 800
            y = (row .+ dy) / 400
        return . Ray white (Vector 0 0 0) $
            Vector (-2) (-1) (-1)
            `plus` x *^ (Vector 4 0 0)
            `plus` y *^ (Vector 0 2 0)

main :: IO ()
main = do
    putStrLn (header w h)
    traverse_ (render world >=> putStrLn . serialize)
        [ (col, row) | row <- [h - 1, h - 2..0], col <- [0..w - 1] ]
  where
    w = 800
    h = 400
    world = fromList
        [ metalSphere (Sphere (Vector (-0.5) 0 (-1)) 0.5) (Vector 0.8 0.6 0.2)
        , metalSphere (Sphere (Vector 0.5 0 (-1)) 0.5) (Vector 0.8 0.8 0.8)
        , lambSphere (Sphere (Vector 0 (-100.5) (-1)) 100) (Vector 0.8 0.8 0)
        ]
