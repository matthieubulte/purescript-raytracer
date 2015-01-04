module Pixel
    ( Pixel()
    , point
    , dist
    ) where

import Math

type Pixel = 
    { x :: Number
    , y :: Number
    }

point :: Number -> Number -> Pixel
point x y = { x:x, y:y }

dist :: Pixel -> Pixel -> Number
dist p1 p2 = sqrt ( ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y)) )
