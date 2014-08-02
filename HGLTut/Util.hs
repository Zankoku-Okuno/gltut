module HGLTut.Util (die, pass, module X) where

import System.IO
import System.Exit
import Control.Applicative as X
import Control.Monad as X


die :: String -> IO () -> IO a
die msg cleanup = do
    hPutStrLn stderr msg
    cleanup
    exitFailure

pass :: Monad m => m ()
pass = return ()