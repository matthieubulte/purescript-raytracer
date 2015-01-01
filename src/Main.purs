module Main where

import Control.Monad.Eff
import Control.Timer

import Data.Maybe
import Data.Either
import Debug.Trace
import DOM

import Graphics.Canvas
import Math
import Signal
import Signal.DOM

import Color
import Point
import FrameBuffer

-- CONSTANTS
width :: Number
width = 1000

height :: Number
height = 500

renderRatio :: Number
renderRatio = 0.35
-- END CONSTANTS

dist :: Point -> Point -> Number
dist p1 p2 = sqrt ( ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y)) )

adjust :: Number -> Number -> Number -> Either Number Number
adjust min max x = let z = (x - min) / (max - min)
                       adjust' k | k < 0 = Left min
                       adjust' k | k > 1 = Left max
                       adjust' _         = Right x

                   in adjust' z


-- MODEL
type BallState =
    { position :: Point
    , velocity :: Point
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
        { position: point 0 100
        , velocity: point 0.15 0.25
        , radius: 10
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

updateComponent :: (Point -> Number) -> (Point -> Number -> Point) -> Number -> BallState -> Number -> BallState
updateComponent get set max ball dt = let uncheckedNew = (get ball.position) + (get ball.velocity) * dt
                                          checkedNew = adjust 0 max uncheckedNew

                                          new = either id id checkedNew
                                          direction = case checkedNew of
                                                           Left  _ -> -1
                                                           Right _ ->  1

                                          newV = direction * (get ball.velocity)

                                      in ball
                                      { position = set ball.position new 
                                      , velocity = set ball.velocity newV
                                      }

updateX = updateComponent (\p -> p.x) (\p n -> p { x=n }) width
updateY = updateComponent (\p -> p.y) (\p n -> p { y=n }) height

updateBall :: Number -> BallState -> BallState
updateBall dt ball = let ball' = updateX ball dt
                     in updateY ball' dt
-- END UPDATE 

-- VIEW
drawBall :: BallState -> Point -> Color
drawBall b p =if (dist b.position p) <= b.radius 
                 then b.color
                 else rgb 123 123 123

draw :: forall e.  FrameBuffer -> State -> Eff (canvas :: Canvas | e) Unit
draw frameBuffer s = do
    updateFrameBuffer frameBuffer renderRatio (drawBall s.ball)
    renderFrameBuffer frameBuffer
-- END VIEW


mainFn :: forall e. CanvasElement -> Eff (trace :: Trace, dom :: DOM, timer :: Timer, canvas :: Canvas | e) Unit
mainFn canvas = do
    tick <- animationFrame

    ctx <- getContext2D canvas
    frameBuffer <- createFrameBuffer ctx width height
    
    runSignal $ foldp updateState initialState tick ~> \state -> do
        draw frameBuffer state

main :: forall e. Eff (trace :: Trace, dom :: DOM, timer :: Timer, canvas :: Canvas | e) Unit
main = do
    maybeCanvas <- getCanvasElementById "canvas"
    case maybeCanvas of
         Just canvas -> mainFn canvas
         Nothing     -> trace "Can't find canvas."
