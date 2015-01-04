module Main where

import Control.Monad.Eff
import Control.Timer

import Data.Either
import Data.Maybe
import Debug.Trace
import DOM

import Graphics.Canvas
import Signal
import Signal.DOM

import Color
import Pixel
import FrameBuffer

import Math.Extra

-- CONSTANTS
width :: Number
width = 200

height :: Number
height = 200

renderRatio :: Number
renderRatio = 0.65

background :: Color
background = rgb 123 123 123
-- END CONSTANTS

-- MODEL
type BallState =
    { position :: Point
    , velocity :: Vector
    , radius :: Number
    , color :: Color
    }

type State = 
    { dt :: Number
    , currentTime :: Number
    , ball :: BallState
    }

initialState :: State
initialState = 
    { dt: 0
    , currentTime: 0
    , ball:
        { position: v3 50 40 0
        , velocity: v3 0 0 0
        , radius: 5
        , color: rgb 123 33 80
        }
    }
-- END MODEL

-- UPDATE
updateState :: Number -> State -> State
updateState time state = let dt = time - state.currentTime
                         in state
                         { dt = dt
                         , currentTime = time
                         , ball = updateBall state.dt state.ball
                         }

-- updateComponent :: (Pixel -> Number) -> (Pixel -> Number -> Pixel) -> Number -> BallState -> Number -> BallState
-- updateComponent get set max ball dt = let uncheckedNew = (get ball.position) + (get ball.velocity) * dt
--                                           checkedNew = adjust 0 max uncheckedNew
-- 
--                                           new = either id id checkedNew
--                                           direction = case checkedNew of
--                                                            Left  _ -> -1
--                                                            Right _ ->  1
-- 
--                                           newV = direction * (get ball.velocity)
-- 
--                                       in ball
--                                       { position = set ball.position new 
--                                       , velocity = set ball.velocity newV
--                                       }
-- 
-- updateX = updateComponent (\p -> p.x) (\p n -> p { x=n }) width
-- updateY = updateComponent (\p -> p.y) (\p n -> p { y=n }) height
-- 
gravity :: Number -> BallState -> BallState
gravity dt ball = let 
                      ny  = ball.position.y + ball.velocity.y * dt
                      nvy = ball.velocity.y - 0.008 * dt

                      collGround = ny < ball.radius
                      nny = if collGround then ball.radius else ny
                      nnvy = if collGround then -nvy * 0.8 else nvy

                   in ball { position = ball.position { y=nny }
                           , velocity = ball.position { y=nnvy}  }

updateBall :: Number -> BallState -> BallState
updateBall dt ball = gravity 16 ball
-- END UPDATE 

-- VIEW
-- drawBall :: BallState -> Pixel -> Color
-- drawBall b p =if (dist b.position p) <= b.radius 
--                  then b.color
--                  else background
-- 
-- draw :: forall e.  FrameBuffer -> State -> Eff (canvas :: Canvas | e) Unit
-- draw frameBuffer s = do
--     updateFrameBuffer frameBuffer renderRatio (drawBall s.ball)
--     renderFrameBuffer frameBuffer
-- END VIEW

import Raytracer
import Math.Extra.Vector

view :: View
view = 
    { position:        v3 (-20) 20 0
    , lookingAt:       v3 0 20 0
    , viewUp:          v3 0 1 0
    , viewingDistance: 100
    , width:           width
    , height:          height
    }

renderPixel :: (Pixel -> Point) -> Shape -> Pixel -> Color
renderPixel vt s p = 
    let maybeIntersection = _closestIntersection view (vt p) s
    in case maybeIntersection of
            Just _  -> rgb 255 0 0
            Nothing -> rgb 0 0 0

render :: forall e. FrameBuffer -> State -> Eff (canvas :: Canvas | e) Unit
render fb s = do
    let vt = _makeViewTransform view
    updateFrameBuffer fb renderRatio (renderPixel vt (Sphere s.ball.radius s.ball.position))
    renderFrameBuffer fb

mainFn :: forall e. CanvasElement -> Eff (trace :: Trace, dom :: DOM, timer :: Timer, canvas :: Canvas | e) Unit
mainFn canvas = do
    tick <- animationFrame

    ctx <- getContext2D canvas
    frameBuffer <- createFrameBuffer ctx width height
    
    runSignal $ foldp updateState initialState tick ~> \state -> do
        render frameBuffer state
        trace $ show state.dt

main :: forall e. Eff (trace :: Trace, dom :: DOM, timer :: Timer, canvas :: Canvas | e) Unit
main = do
    maybeCanvas <- getCanvasElementById "canvas"
    case maybeCanvas of
         Just canvas -> mainFn canvas
         Nothing     -> trace "Can't find canvas."
