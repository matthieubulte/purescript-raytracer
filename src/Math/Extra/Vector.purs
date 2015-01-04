module Math.Extra.Vector
    ( Vector (..)
    , v3
    , add
    , sub
    , squared_mag
    , mag
    , mult
    , dot
    , cross
    , neg
    , normalize
    ) where

import Math (sqrt)

type Vector = 
    { x :: Number
    , y :: Number
    , z :: Number
    }

v3 :: Number -> Number -> Number -> Vector
v3 x y z = { x:x, y:y, z:z }

add :: Vector -> Vector -> Vector
add v v' = v3 (v.x+v'.x) (v.y+v'.y) (v.z+v'.z)

sub :: Vector -> Vector -> Vector
sub v v' = v3 (v.x-v'.x) (v.y-v'.y) (v.z-v'.z)

dot :: Vector -> Vector -> Number
dot v v' = (v.x*v'.x) + (v.y*v'.y) + (v.z*v'.z)

squared_mag :: Vector -> Number
squared_mag v = dot v v

mag :: Vector -> Number
mag = sqrt <<< squared_mag

mult :: Vector -> Number -> Vector
mult v k = v3 (k*v.x) (k*v.y) (k*v.z)

cross :: Vector -> Vector -> Vector
cross v v' = v3 (v.y*v'.z + v.z*v'.y)  (-(v.x*v'.z + v.z*v'.x)) (v.x*v'.y + v.y*v'.x)

neg :: Vector -> Vector
neg v = v3 (-v.x) (-v.y) (-v.z)

normalize :: Vector -> Vector
normalize v = let m = mag v
              in case m of
                  0         -> v
                  otherwise -> v `mult` (1 / m)
