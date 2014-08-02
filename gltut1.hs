module Main (main) where


import HGLTut.Util
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLclampf)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (WindowHint(..), OpenGLProfile(..), Key(..), KeyState(..))


main :: IO ()
main = do
    initOrDie
    mapM_ GLFW.windowHint [ WindowHint'Samples 4
                          , WindowHint'ContextVersionMajor 3
                          , WindowHint'ContextVersionMinor 2
                          , WindowHint'OpenGLForwardCompat True
                          , WindowHint'OpenGLProfile OpenGLProfile'Core
                          ]
    -- FIXME figure out window size dynamically
    window <- windowOrDie (1366, 768) "Tutorial 1"
    -- TODO initialize GLEW
    GL.clearColor $= GL.Color4 (0::GLclampf) 0 0 0    
    loop window
    GLFW.terminate


loop :: GLFW.Window -> IO ()
loop window = do
    -- perform rendering
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    display window
    GLFW.swapBuffers window
    -- respond to user input
    GLFW.waitEvents
    stop <- respond window
    shouldClose <- GLFW.windowShouldClose window
    -- loop
    unless (stop || shouldClose) $ loop window 

display :: GLFW.Window -> IO ()
display window = return ()

respond :: GLFW.Window -> IO Bool
respond window = do
    quit <- (== KeyState'Pressed) <$> GLFW.getKey window Key'Escape
    return quit

handleError :: GLFW.Error -> String -> IO ()
handleError err msg = die ("GLFW failure " ++ show err ++ ": " ++ msg) GLFW.terminate


initOrDie :: IO ()
initOrDie = do
    GLFW.setErrorCallback $ Just handleError
    success <- GLFW.init
    if True
        then return ()
        else die "Failed to initialize GLFW." pass

windowOrDie :: (Int, Int) -> String -> IO GLFW.Window
windowOrDie (width, height) title = do
    m_window <- GLFW.createWindow width height title Nothing Nothing
    case m_window of
        Just window -> do
            GLFW.makeContextCurrent $ Just window
            configureInputMode window
            return window
        Nothing -> die "Failed to open GLFW window." GLFW.terminate

configureInputMode :: GLFW.Window -> IO ()
configureInputMode window = do
    GLFW.setCursorInputMode window GLFW.CursorInputMode'Normal
    --GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled
    --GLFW.setStickyMouseButtonsInputMode window GLFW.StickyMouseButtonsInputMode'Enabled
