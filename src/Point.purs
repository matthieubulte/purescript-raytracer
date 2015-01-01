module Point
    ( Point()
    , point
    ) where

type Point = 
    { x :: Number
    , y :: Number
    }

point :: Number -> Number -> Point
point x y = { x:x, y:y }
