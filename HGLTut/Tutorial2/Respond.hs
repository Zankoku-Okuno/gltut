module Tutorial2.Respond (respond) where

import Util
--import qualified Graphics.Rendering.OpenGL as GL
--import Graphics.Rendering.OpenGL (($=), GLclampf)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Key(..), KeyState(..))


respond :: GLFW.Window -> IO Bool
respond window = do
    quit <- (== KeyState'Pressed) <$> GLFW.getKey window Key'Escape
    return quit