module Math.Extra.MutVector
    ( mutAdd
    , mutSub
    , mutMult
    , mutCross
    , mutNeg
    , mutNormalize
    ) where

import Math (sqrt)
import Data.Function
import Math.Extra.Vector

foreign import mutAddX """
function mutAddX(v1, v2, to) {
    to.x = v1.x + v2.x;
    to.y = v1.y + v2.y;
    to.z = v1.z + v2.z;

    return to;
}""" :: Fn3 Vector Vector Vector Vector

mutAdd :: Vector -> Vector -> Vector -> Vector
mutAdd = runFn3 mutAddX

foreign import mutSubX """
function mutSubX(v1, v2, to) {
    to.x = v1.x - v2.x;
    to.y = v1.y - v2.y;
    to.z = v1.z - v2.z;

    return to;
}""" :: Fn3 Vector Vector Vector Vector

mutSub :: Vector -> Vector -> Vector -> Vector
mutSub = runFn3 mutSubX

foreign import mutMultX """
function mutMultX(v1, k, to) {
    to.x = v1.x * k;
    to.y = v1.y * k;
    to.z = v1.z * k;

    return to;
}""" :: Fn3 Vector Number Vector Vector

mutMult :: Vector -> Number -> Vector -> Vector
mutMult = runFn3 mutMultX

foreign import mutCrossX """
function mutCrossX(v1, v2, to) {
    to.x = v1.y*v2.z + v1.z*v2.y;
    to.y = -(v1.y*v2.z + v1.z*v2.x);
    to.z = v1.x*v2.y + v1.y*v2.x;

    return to;
}""" :: Fn3 Vector Vector Vector Vector

mutCross :: Vector -> Vector -> Vector -> Vector
mutCross = runFn3 mutCrossX

foreign import mutNegX """
function mutNegX(v1, k, to) {
    to.x = -v1.x;
    to.y = -v1.y;
    to.z = -v1.z;

    return to;
}""" :: Fn3 Vector Number Vector Vector

mutNeg :: Vector -> Number -> Vector -> Vector
mutNeg = runFn3 mutNegX

foreign import mutNormalizeX """
function mutNormalizeX(v1, to) {
    var m = Math.sqrt(v1.x*v1.x + v1.y*v1.y + v1.z*v1.z);
    if(m === 0) {
       m = 1;
    }

    to.x = v1.x / m;
    to.y = v1.y / m;
    to.z = v1.z / m;

    return to;
}""" :: Fn2 Vector Vector Vector

mutNormalize :: Vector -> Vector -> Vector
mutNormalize = runFn2 mutNormalizeX
