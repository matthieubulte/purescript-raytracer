module Point
    ( Point()
    , point
    , dist
    ) where

import Math

type Point = 
    { x :: Number
    , y :: Number
    }

point :: Number -> Number -> Point
point x y = { x:x, y:y }

dist :: Point -> Point -> Number
dist p1 p2 = sqrt ( ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y)) )
