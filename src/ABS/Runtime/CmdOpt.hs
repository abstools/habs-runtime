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
  , http_port :: Int
                     } deriving (Data, Typeable)

cmdOptSpec :: CmdOpt
cmdOptSpec = CmdOpt { 
    trace_exceptions = def &= help "The COGs hide any uncaught exceptions by default. This option logs to the stdout an uncaught exception. Used for debugging"
  , http_port = 8080 &=name "p" &= typ "NUM" &= help "The port that the REST HTTP server listens on. Defaults to 8080"
             }
             &= program "habs-runtime" 
             &= help "The ABS-Haskell runtime" 
             &= helpArg [explicit, name "h", name "help"]
             &= summary "The ABS-Haskell runtime, Nikolaos Bezirgiannis, Envisage Project" -- summary is --version
