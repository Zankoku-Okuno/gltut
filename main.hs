import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  let num = if null _args then 12 else read (head _args)
  _window <- createWindow "Hello World"
  displayCallback $= display num
  mainLoop
 
display :: Int -> DisplayCallback
display num = do
  clear [ ColorBuffer ]
  renderPrimitive TriangleStrip $
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (myPoints num)
  flush

myPoints :: Int -> [(GLfloat,GLfloat,GLfloat)]
myPoints num = [ (0.8*sin(2*pi*(fromIntegral k)/(fromIntegral num)), 0.8*cos(2*pi*(fromIntegral k)/(fromIntegral num)), 0) | k <- [1..num] ]
