module Main (main) where

import Debug.Trace

import HGLTut.Util
import HGLTut.Util.VAO
import HGLTut.Util.VBO
import HGLTut.Init
import HGLTut.Loop

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (WindowHint(..), OpenGLProfile(..), Key(..), KeyState(..))

import Foreign.Ptr

main :: IO ()
main = do
    withGLFW $ \window -> do
        prog <- loadProgram


        [vao] <- GL.genObjectNames 1
        GL.bindVertexArrayObject $= Just vao
        
        [vbo] <- GL.genObjectNames 1
        GL.bindBuffer GL.ArrayBuffer $= Just vbo
        bufferList GL.ArrayBuffer points GL.StaticDraw
        
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 (offsetPtr 0));
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

        GL.bindBuffer GL.ArrayBuffer $= Nothing
        GL.bindVertexArrayObject $= Nothing
      

        loop (display prog vao) respond window

display :: GL.Program -> VAO -> GLFW.Window -> IO ()
display prog vao window = do
    GL.currentProgram $= Just prog
    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 3
    GL.bindVertexArrayObject $= Nothing
    GL.currentProgram $= Nothing
            

respond :: GLFW.Window -> IO ()
respond window = do
    quit <- (== KeyState'Pressed) <$> GLFW.getKey window Key'Escape
    when quit $ GLFW.setWindowShouldClose window True

points :: [GLfloat]
points = [ -1.0, -1.0, 0.0
         ,  1.0, -1.0, 0.0
         ,  0.0,  1.0, 0.0
         ]

offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral


loadVertexShader :: IO GL.VertexShader
loadVertexShader = do
    [vert] <- GL.genObjectNames 1
    vertSource <- readFile "gltut2.vert.glsl"
    GL.shaderSource vert $= [vertSource]
    GL.compileShader vert
    return vert

loadFragmentShader :: IO GL.FragmentShader
loadFragmentShader = do
    [frag] <- GL.genObjectNames 1
    fragSource <- readFile "gltut2.frag.glsl"
    GL.shaderSource frag $= [fragSource]
    GL.compileShader frag
    return frag

loadProgram :: IO GL.Program
loadProgram = do
    [prog] <- GL.genObjectNames 1
    vert <- loadVertexShader
    frag <- loadFragmentShader
    GL.attachedShaders prog $= ([vert], [frag])
    GL.linkProgram prog
    report <- GL.get (GL.programInfoLog prog)
    trace report pass
    return prog




