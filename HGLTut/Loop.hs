module HGLTut.Loop (loop) where

import HGLTut.Util
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

loop :: (GLFW.Window -> IO ()) -- ^ display function
     -> (GLFW.Window -> IO ()) -- ^ respond function (returns whether the loop should stop)
     -> GLFW.Window -- ^ window to operate in
     -> IO ()
loop display respond window = do
    -- perform rendering
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    display window
    GLFW.swapBuffers window
    -- respond to user input
    GLFW.waitEvents
    () <- respond window
    -- loop
    shouldClose <- GLFW.windowShouldClose window
    unless shouldClose $ loop respond display window