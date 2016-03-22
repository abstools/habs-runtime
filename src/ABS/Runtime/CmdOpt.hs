{-# LANGUAGE DeriveDataTypeable #-}
-- | Possible options passed to The ABS-Haskell runtime-system
module ABS.Runtime.CmdOpt where

import System.Console.CmdArgs
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE cmdOpt #-}
cmdOpt :: CmdOpt
cmdOpt = unsafePerformIO (cmdArgs cmdOptSpec)

data CmdOpt = CmdOpt {
      keep_alive :: Bool -- ^ If enabled, the ABS-program will keep running even after the end of the main-block process
    , trace_exceptions :: Bool -- ^ The COGs hide any uncaught exceptions by default. This option logs to the stdout an uncaught exception. Used for debugging
    } deriving (Show, Data, Typeable)

cmdOptSpec :: CmdOpt
cmdOptSpec = CmdOpt { 
               keep_alive = def &= help "If enabled, the ABS-program will keep running even after the end of the main-block process"
             , trace_exceptions = def &= help "The COGs hide any uncaught exceptions by default. This option logs to the stdout an uncaught exception. Used for debugging"
             }
             &= program "habs-runtime" 
             &= help "The ABS-Haskell runtime" 
             &= helpArg [explicit, name "h", name "help"]
             &= summary ("The ABS-Haskell runtime, Nikolaos Bezirgiannis, Envisage Project") -- summary is --version
