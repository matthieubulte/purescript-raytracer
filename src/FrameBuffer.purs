module FrameBuffer
    ( FrameBuffer (..)
    , createFrameBuffer
    , updateFrameBuffer
    , renderFrameBuffer
    ) where

import Control.Monad.Eff
import Data.Function
import Graphics.Canvas

import Color
import Pixel

foreign import data FrameBuffer :: *

foreign import createFrameBufferV """
function createFrameBufferV(ctx2d, width, height) {
    return function() {
        var imageData = ctx2d.createImageData(width, height);

        function render() {
            ctx2d.putImageData(imageData, 0, 0);
        }

        return {
            data: imageData.data,
            width: width,
            height: height,
            render: render,
            sharedPixel: {x:0, y:0}
        };
    };
}""" :: forall e. Fn3 Context2D Number Number (Eff e FrameBuffer)

createFrameBuffer :: forall e. Context2D -> Number -> Number -> Eff e FrameBuffer
createFrameBuffer = runFn3 createFrameBufferV

foreign import updateFrameBufferV """
    function updateFrameBufferV(fb, ratio, fn) {
        return function() {
            var n;
            var x;
            var index;
            var pixels = fb.data.length / 4;
            var toCompute = pixels * ratio;

            while(toCompute--) {
                n = Math.floor(Math.random() * pixels);
                index = n * 4;

                fb.sharedPixel.x = n % fb.width;
                fb.sharedPixel.y = (n-fb.sharedPixel.x) / fb.width;
                
                x = fn(fb.sharedPixel);
                fb.data[index++] = (x >> 24) & 0xff;
                fb.data[index++] = (x >> 16) & 0xff;
                fb.data[index++] = (x >>  8) & 0xff;   
                fb.data[index++] =  x        & 0xff;
            }
        }    
    }""" :: forall e. Fn3 FrameBuffer Number (Pixel -> Color) (Eff e Unit)

updateFrameBuffer :: forall e. FrameBuffer -> Number -> (Pixel -> Color) -> Eff e Unit
updateFrameBuffer = runFn3 updateFrameBufferV

foreign import renderFrameBuffer """
function renderFrameBuffer(fb) {
    return function() {
       fb.render();
    }
}""" :: forall e. FrameBuffer -> Eff e Unit
