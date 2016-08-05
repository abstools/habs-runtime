{-# LANGUAGE DeriveDataTypeable #-}
-- | Possible options passed to The ABS-Haskell runtime-system
module ABS.Runtime.CmdOpt where

import System.Console.CmdArgs
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE cmdOpt #-}
cmdOpt :: CmdOpt
cmdOpt = unsafePerformIO (cmdArgs cmdOptSpec)

data CmdOpt = CmdOpt {
    trace_exceptions :: Bool -- ^ The COGs hide any uncaught exceptions by default. This option logs to the stdout an uncaught exception. Used for debugging
  , ip :: String
  , port :: String
  , creator :: String
                     } deriving (Data, Typeable)

cmdOptSpec :: CmdOpt
cmdOptSpec = CmdOpt { 
    trace_exceptions = def &= help "The COGs hide any uncaught exceptions by default. This option logs to the stdout an uncaught exception. Used for debugging"
  , ip = "127.0.0.1" &= help "The IP address where this-DC connects through network to other DCs."
  , port = "10501" &= help "The Port address to listen to (N.B. must not be blocked by the firewall)"
  , creator = def &= help "The creator-DC (given in a serialized format) of this machine to respond back to. If omitted, means this-DC is the creator so it should run main block."
             }
             &= program "habs-runtime" 
             &= help "The ABS-Haskell runtime" 
             &= helpArg [explicit, name "h", name "help"]
             &= summary "The ABS-Haskell runtime, Nikolaos Bezirgiannis, Envisage Project" -- summary is --version
