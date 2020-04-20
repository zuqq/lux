{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (for_)
import Data.List     (intercalate)

import qualified Streaming         as S
import qualified Streaming.Prelude as SP

import Lux.Render
import Lux.Vector


-- | PPM header.
header :: Scene -> String
header scene = intercalate "\n" ["P3", show w <> " " <> show h, "255"]
  where
    h = sHeight scene
    w = sWidth scene

serialize :: Vector -> String
serialize (Vector r g b) = intercalate " " $
    show . floor . (255.99 *) <$> [r, g, b]

defaultScene :: Scene
defaultScene = Scene
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

render :: Scene -> S.Stream (S.Of Vector) IO ()
render scene =
    for_ [h - 1, h - 2..0] $ \j ->
        for_ [0..w - 1] $ \i ->
            S.liftIO (withAA scene i j) >>= SP.yield
  where
    h = sHeight scene
    w = sWidth scene

main :: IO ()
main = SP.stdoutLn
    . SP.cons (header scene)
    . SP.map serialize
    . render $ scene
  where
    scene = defaultScene
