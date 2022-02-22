module Slurm.Init
  ( initSlurm
  , finiSlurm
  , withSlurm
  ) where

import           Control.Exception (bracket_)
import           Foreign.C.String (CString, withCString)
import           Foreign.Marshal.Utils (maybeWith)

#include <slurm/slurm.h>

foreign import ccall unsafe slurm_init :: CString -> IO ()
foreign import ccall unsafe slurm_fini :: IO ()

initSlurm :: Maybe String -> IO ()
initSlurm conf = maybeWith withCString conf slurm_init

finiSlurm :: IO ()
finiSlurm = slurm_fini

withSlurm :: Maybe String -> IO a -> IO a
withSlurm conf = bracket_ (initSlurm conf) finiSlurm
