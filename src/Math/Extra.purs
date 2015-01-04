module Math.Extra
    ( adjust
    , roots
    ) where

import Data.Either
import Math

adjust :: Number -> Number -> Number -> Either Number Number
adjust min max x = let z = (x - min) / (max - min)
                       adjust' k | k < 0 = Left min
                       adjust' k | k > 1 = Left max
                       adjust' _         = Right x

                   in adjust' z

roots :: Number -> Number -> Number -> [Number]
roots a b c = let det = b*b - 4*a*c
              in roots' det
                where roots' 0         = [(-0.5 * b)]
                      roots' d | d > 0 = [(0.5 * (-b + sqrt d)), (0.5 * (-b - sqrt d))]
                      roots' _         = []
