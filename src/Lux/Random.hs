module Lux.Random
    ( Random
    , sample
    ) where


import Control.Monad.Random.Strict (Rand, liftRand)
import System.Random               (StdGen, UniformRange, uniformR)

type Random = Rand StdGen

sample :: UniformRange a => (a, a) -> Random a
sample r = liftRand $ uniformR r
