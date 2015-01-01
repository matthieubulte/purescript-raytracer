module Color 
  ( Color()
  , rgba
  , rgb
  ) where

import Data.Function

foreign import data Color :: *

foreign import rgbaV """
    function rgbaV(r, g, b, a) {
        return ((r & 0xff) << 24)
             | ((g & 0xff) << 16)
             | ((b & 0xff) <<  8)
             |  (a & 0xff);
    }""" :: Fn4 Number Number Number Number Color

rgba :: Number -> Number -> Number -> Number -> Color
rgba = runFn4 rgbaV

foreign import rgbV """
    function rgbV(r, g, b) {
        return ((r & 0xff) << 24)
             | ((g & 0xff) << 16)
             | ((b & 0xff) <<  8)
             |  0xff;
    }""" :: Fn3 Number Number Number Color

rgb :: Number -> Number -> Number -> Color
rgb = runFn3 rgbV
