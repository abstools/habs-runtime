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
  , port :: Maybe Int
  , unit_time :: (Int, TimeUnit)
                     } deriving (Data, Typeable)

data TimeUnit = S | Ms | Us
    deriving (Show, Data, Typeable)

cmdOptSpec :: CmdOpt
cmdOptSpec = CmdOpt { 
    trace_exceptions = def &= help "The COGs hide any uncaught exceptions by default. This option logs to the stdout an uncaught exception. Used for debugging"
  , port = def &= typ "NUM" &= help "The port that the REST HTTP server listens on."
  , unit_time = (1,S) &= typ "NUM,(s|ms|us)" &= help "Set the time unit for the ABS real-time extension. Defaults to: 1,s" 
             }
             &= program "habs-runtime" 
             &= help "The ABS-Haskell runtime" 
             &= helpArg [explicit, name "h", name "help"]
             &= summary "The ABS-Haskell runtime, Nikolaos Bezirgiannis, Envisage Project" -- summary is --version
