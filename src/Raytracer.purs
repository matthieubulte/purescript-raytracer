module Raytracer 
    ( Point()
    , Direction()
    , Time()
    , Ray()

    , Radius()
    , Normal()
    , Shape(..)
    , Intersection(..)
    , TimedIntersection()
    , closestIntersection
    , _closestIntersection

    , View (..)
    , pointToRay
    , makeViewTransform
    , _makeViewTransform
    ) where

import Math.Extra
import Math.Extra.MutVector
import Math.Extra.Vector
import Data.Tuple
import Data.Array (filter, map)
import Data.Foldable (foldlArray)
import Data.Maybe
import Pixel

type Point = Vector
type Direction = Vector
type Time = Number
type Ray = Tuple Point Direction

positionAtTime :: Ray -> Time -> Point
positionAtTime (Tuple s d) t = s `add` (d `mult` t)

type Normal = Vector
type Radius = Number

data Shape = Sphere Radius Vector

type Intersection = 
    { point  :: Point
    , normal :: Normal
    , ray    :: Ray
    }

type TimedIntersection = Tuple Time Intersection
epsilon :: Number
epsilon = 0.001

-- roots :: Number -> Number -> Number -> [Number]
-- roots a b c = let det = b*b - 4*a*c
--               in roots' det
--                 where roots' 0         = [(-0.5 * b)]
--                       roots' d | d > 0 = [(0.5 * (-b + sqrt d)), (0.5 * (-b - sqrt d))]
--                       roots' _         = []

import Data.Function

--    pointToRay :: View -> Point -> Ray
--    pointToRay { position = cp } p = Tuple cp (normalize (p `sub` cp))
foreign import _closestIntersectionX """
function _closestIntersectionX(v3, sub, normalize, squared_mag, dot, view, point, shape) {
    var base = point;
    var dir = v3(0)(0)(0);

    sub(point)(view.position)(dir);
    normalize(dir)(dir);
    
    var radius = shape.value0;
    var center = shape.value1;

    var baseSubCenter = sub(base)(center)(base);
    var a = squared_mag(dir);
    var b = 2 * dot(dir)(baseSubCenter);
    var c = squared_mag(baseSubCenter) - radius * radius;

    var det = b*b - 4*a*c;
    if(det < 0) {
       return PS.Data_Maybe.Nothing.value;
    }

    var t = undefined;
    
    if(det === 0) {
      t = -0.5*b; 
    }

    else if(det > 0) {
        var rdet = Math.sqrt(det);
        var x0 = 0.5 * (-b + rdet);
        var x1 = 0.5 * (-b - rdet);

        t = x0 < x1 ? x0 : x1;
    }

    return PS.Data_Maybe.Just.create({
        point: null,
        normal: null,
        ray: null
    });
   
}""" :: Fn8 (Number -> Number -> Number -> Vector)
            (Vector -> Vector -> Vector -> Vector)
            (Vector -> Vector -> Vector)
            (Vector -> Number)
            (Vector -> Vector -> Number)
            View Point Shape (Maybe Intersection)

_closestIntersection :: View -> Point -> Shape -> Maybe Intersection
_closestIntersection = (runFn8 _closestIntersectionX) (v3) (mutSub) (mutNormalize) (squared_mag) (dot)

intersections :: Ray -> Shape -> [TimedIntersection]
intersections ray@(Tuple base dir) (Sphere radius center) = 
    let base_sub_center = base `sub` center
        a = squared_mag dir
        b = 2 * (dir `dot` base_sub_center)
        c = (squared_mag base_sub_center) - radius*radius

        intersecTimes = filter ((<=) epsilon) (roots a b c)
        normalAtTime t = normalize ( (positionAtTime ray t) `sub` center )

        intersectionAtTime t = 
           Tuple t 
           ({ point:  positionAtTime ray t
            , normal: normalAtTime t
            , ray:    ray
            }) 

    in map intersectionAtTime intersecTimes


closest :: [TimedIntersection] -> Maybe Intersection
closest [] = Nothing
closest (x:xs) =
    let happenedFirst ti1@(Tuple t1 _) ti2@(Tuple t2 _) = if t2 < t1 then ti2 else ti1
    in Just $ snd (foldlArray happenedFirst x xs)

closestIntersection :: Ray -> Shape -> Maybe Intersection
closestIntersection r s = closest $ intersections r s


type View = 
    { position        :: Point
    , lookingAt       :: Point
    , viewUp          :: Direction
    , viewingDistance :: Number
    , width           :: Number
    , height          :: Number
    }

pointToRay :: View -> Point -> Ray
pointToRay { position = cp } p = Tuple cp (normalize (p `sub` cp))

foreign import _makeViewTransformX """
function _makeViewTransformX(v3, normalize, sub, add, mult, cross, view) {

    var viewDir = v3(0)(0)(0);
    sub(view.lookingAt)(view.position)(viewDir);
    normalize(viewDir)(viewDir);

    var screenCenter = v3(0)(0)(0);
    mult(viewDir)(view.viewingDistance)(screenCenter);
    add(view.position)(screenCenter)(screenCenter);

    var viewRight = v3(0)(0)(0);
    cross(viewDir)(view.viewUp)(viewRight);

    return function(p) {
        var x = p.x - 0.5*view.width;
        var y = p.y - 0.5*view.height;
        var z = 0;

        return v3 (screenCenter.x + (viewRight.x * x) + (view.viewUp.x * (-y)))
                  (screenCenter.y + (viewRight.y * x) + (view.viewUp.y * (-y)))
                  (screenCenter.z + (viewRight.z * x) + (view.viewUp.z * (-y)));
    }
}""" :: Fn7 (Number -> Number -> Number -> Vector)
           (Vector -> Vector -> Vector)
           (Vector -> Vector -> Vector -> Vector)
           (Vector -> Vector -> Vector -> Vector)
           (Vector -> Number -> Vector -> Vector)
           (Vector -> Vector -> Vector -> Vector)
           View (Pixel -> Point)

_makeViewTransform :: View -> (Pixel -> Point)
_makeViewTransform = (runFn7 _makeViewTransformX) (v3) (mutNormalize) (mutSub) (mutAdd) (mutMult) (mutCross)

makeViewTransform :: View -> (Pixel -> Point)
makeViewTransform view = 
    let pToV p = v3 (p.x - 0.5*view.width) (p.y - 0.5*view.height) 0
        viewDir = normalize (view.lookingAt `sub` view.position)
        screenCenter = view.position `add` (viewDir `mult` view.viewingDistance)
        viewRight = viewDir `cross` view.viewUp
        transform v = screenCenter `add` (viewRight `mult` v.x) `add` (view.viewUp `mult` (-v.y))

     in transform <<< pToV
