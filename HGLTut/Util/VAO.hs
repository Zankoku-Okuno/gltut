-- | A thin layer over OpenGL 3.1+ vertex array objects.
module HGLTut.Util.VAO 
  (makeVAO, withVAO, deleteVAO, deleteVAOs, VAO) where
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31 (glDeleteVertexArrays)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Unsafe.Coerce (unsafeCoerce)

-- |Short alias for 'VertexArrayObject'.
type VAO = VertexArrayObject

-- |Allocate a 'VertexArrayObject', and initialize it with the
-- provided action. This action should bind the buffer data, index
-- data (if necessary), and setup vertex attributes.
makeVAO :: IO () -> IO VAO
makeVAO setup = do
    [vao] <- genObjectNames 1
    _ <- withVAO vao setup
    return vao

-- |Run an action with the given 'VertexArrayObject' bound.
withVAO :: VertexArrayObject -> IO r -> IO r
withVAO vao useIt = do 
    bindVertexArrayObject $= Just vao
    r <- useIt
    bindVertexArrayObject $= Nothing
    return r

-- | Delete a 'VertexArrayObject'.
deleteVAO :: VertexArrayObject -> IO ()
deleteVAO vao = deleteVAOs [vao]

-- | Delete a list of 'VertexArrayObject's.
deleteVAOs :: [VertexArrayObject] -> IO ()
deleteVAOs vaos = withArrayLen (map vaoID vaos) $ 
                    glDeleteVertexArrays . fromIntegral
  where vaoID = unsafeCoerce :: VertexArrayObject -> GLuint