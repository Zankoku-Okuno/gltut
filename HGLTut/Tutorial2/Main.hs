module Tutorial2.Main (main) where

import Util
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLclampf, GLfloat)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (WindowHint(..), OpenGLProfile(..), Key(..), KeyState(..))

import Tutorial2.Init
import Tutorial2.Display
import Tutorial2.Respond


main :: IO ()
main = withGLFW loop

{- create resources
    GLuint VertexArrayID;
    glGenVertexArrays(1, &VertexArrayID);
    glBindVertexArray(VertexArrayID);

    // Create and compile our GLSL program from the shaders
    GLuint programID = LoadShaders( "SimpleVertexShader.vertexshader", "SimpleFragmentShader.fragmentshader" );

    GLuint vertexbuffer;
    glGenBuffers(1, &vertexbuffer);
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);
-}

{-
    // Cleanup VBO
    glDeleteBuffers(1, &vertexbuffer);
    glDeleteVertexArrays(1, &VertexArrayID);
    glDeleteProgram(programID);
-}

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
    if stop || shouldClose then return () else loop window


points :: [GLfloat]
points = [ -1.0f, -1.0f, 0.0f
         ,  1.0f, -1.0f, 0.0f
         ,  0.0f,  1.0f, 0.0f
         ]




