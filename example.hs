-- |
-- Module:       Main
-- Description:  Simple example that acquires lock for a short period of time.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
module Main (main)
    where

import Control.Concurrent (threadDelay)
    -- From base package, but GHC specific.
import Control.Exception as Exception (handle)
import Control.Monad (void)

import Control.Monad.IO.Class (MonadIO(liftIO))
    -- From transformers package
    -- http://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default.Class (Default(def))
    -- From data-default-class package, alternatively it's possible to use
    -- data-default package version 0.5.2 and above.
    -- http://hackage.haskell.org/package/data-default-class
    -- http://hackage.haskell.org/package/data-default
import System.IO.LockFile.Resource
    ( LockingException
    , LockingParameters(retryToAcquireLock)
    , RetryStrategy(No)
    , acquireLockFile
    )


lockFileName :: FilePath
lockFileName = "/var/run/lock/my-example-lock"

main :: IO ()
main = Exception.handle exceptionHandler . runResourceT $ do
    void $ acquireLockFile def{retryToAcquireLock = No} lockFileName
    liftIO $ threadDelay 1000000
  where
    exceptionHandler :: LockingException -> IO ()
    exceptionHandler = putStrLn . ("Locking failed with: " ++) . show
