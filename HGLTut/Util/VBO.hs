{-# LANGUAGE ScopedTypeVariables #-}
module HGLTut.Util.VBO (bufferList) where

import Data.Word (Word32)
import Graphics.Rendering.OpenGL
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Array.Storable


makeBuffer :: IO BufferObject
makeBuffer = do
    [buffer] <- genObjectNames 1
    return buffer

withArrayBuffer :: BufferObject -> IO r -> IO r
withArrayBuffer buffer useIt = do 
    bindBuffer ArrayBuffer $= Just buffer
    r <- useIt
    bindBuffer ArrayBuffer $= Nothing
    return r

loadArrayBuffer :: forall a. Storable a => [a] -> IO ()
loadArrayBuffer vertices = do
    let len = length vertices
        n = fromIntegral $ len * sizeOf (undefined::a)
    arr <- newListArray (0, len - 1) vertices
    withStorableArray arr $ \ptr -> 
        bufferData ArrayBuffer $= (n, ptr, StaticDraw)

bufferList :: forall a. Storable a => BufferTarget -> [a] -> BufferUsage -> IO ()
bufferList target xs usage = do
    let len = length xs
        n = fromIntegral $ len * sizeOf (undefined::a)
    arr <- newListArray (0, len - 1) xs
    withStorableArray arr $ \ptr -> 
        bufferData target $= (n, ptr, usage)    