import System.Exit
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Control.Monad
import Data.IORef

main :: IO ()
main = do
  -- Setup --
  (_progName, _args) <- getArgsAndInitialize
  let num = if null _args then 12 else read (head _args)
  initialDisplayMode $= [WithDepthBuffer]
  angle <- newIORef 0
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)
  -- Create Windows --
  _window <- createWindow "Hello World"
  depthFunc $= Just Less -- important that this comes after a window is created, or else segfault. dunno why
  -- Setup callbacks --
  displayCallback $= display angle num
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse delta)
  idleCallback $= Just (idle angle delta)
  mainLoop

display :: IORef GLfloat -> Int -> DisplayCallback
display angle num = do
--  clear [ColorBuffer]
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  a <- get angle
  rotate a $ Vector3 0 0 1
--  rotate a $ Vector3 0 0.1 1
  scale 0.7 0.7 (0.7 :: GLfloat)
  forM_ (myPoints num) $ \(x, y, z) -> 
    preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 x y z
      rotate 30 $ Vector3 x y z
      cube 0.1
      color $ Color3 0 0 (0 :: GLfloat)
      cubeFrame 0.1
  flush

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing

reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size)

keyboardMouse :: IORef GLfloat -> KeyboardMouseCallback
keyboardMouse delta key Down _ _ = case key of
  (Char ' ') -> delta $~! negate
  (Char '+') -> delta $~! (* 2)
  (Char '-') -> delta $~! (/ 2)
  (Char 'q') -> exitSuccess
  _ -> return ()
keyboardMouse delta _ _ _ _ = return ()

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

--translate3f :: (GLfloat, GLfloat, GLfloat) -> 
--translate3f (x, y, z) = translate $ Vector3 x y z

myPoints :: Int -> [(GLfloat,GLfloat,GLfloat)]
myPoints num = [ (sin(2*pi*(fromIntegral k)/(fromIntegral num)), cos(2*pi*(fromIntegral k)/(fromIntegral num)), 0) | k <- [1..num] ]

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]
