module Lux.Vector where

data Vector = Vector
    { x :: !Double
    , y :: !Double
    , z :: !Double
    }

infixr 5 `dot`
infixr 6 `minus`
infixr 7 *^

infixl 7 /^

instance Semigroup Vector where
    (<>) (Vector x y z) (Vector x' y' z') = Vector (x + x') (y + y') (z + z')

instance Monoid Vector where
    mempty = Vector 0 0 0

minus :: Vector -> Vector -> Vector
minus (Vector x y z) (Vector x' y' z') = Vector (x - x') (y - y') (z - z')

dot :: Vector -> Vector -> Double
dot (Vector x y z) (Vector x' y' z') = x * x' + y * y' + z * z'

len :: Vector -> Double
len v = sqrt $ dot v v

(*^) :: Double -> Vector -> Vector
(*^) a (Vector x y z) = Vector (a * x) (a * y) (a * z)

(/^) :: Vector -> Double -> Vector
(/^) (Vector x y z) a = Vector (x / a) (y / a) (z / a)

unit :: Vector -> Vector
unit v = v /^ len v
