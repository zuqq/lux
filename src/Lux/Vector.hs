module Lux.Vector
    ( (*^)
    , (/^)
    , Vector (..)
    , cross
    , dot
    , len
    , minus
    , plus
    , unit
    ) where


data Vector = Vector
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    deriving Show

infixr 5 `cross`, `dot`
infixr 6 `plus`
infixr 7 *^

infixl 6 `minus`
infixl 7 /^

plus :: Vector -> Vector -> Vector
plus (Vector x y z) (Vector x' y' z') = Vector (x + x') (y + y') (z + z')

minus :: Vector -> Vector -> Vector
minus (Vector x y z) (Vector x' y' z') = Vector (x - x') (y - y') (z - z')

-- | Cross product of two vectors.
--
-- ==== __Examples__
--
-- >>> cross (Vector 1 0 0) (Vector 0 1 0)
-- Vector 0.0 0.0 1.0
-- >>> cross (Vector 1 2 3) (Vector (-7) 8 9)
-- Vector (-6.0) (-30.0) 22.0
cross :: Vector -> Vector -> Vector
cross (Vector x y z) (Vector x' y' z') =
    Vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

dot :: Vector -> Vector -> Double
dot (Vector x y z) (Vector x' y' z') = x * x' + y * y' + z * z'

(*^) :: Double -> Vector -> Vector
(*^) a (Vector x y z) = Vector (a * x) (a * y) (a * z)

(/^) :: Vector -> Double -> Vector
(/^) (Vector x y z) a = Vector (x / a) (y / a) (z / a)

len :: Vector -> Double
len v = sqrt $ dot v v

unit :: Vector -> Vector
unit v = v /^ len v
