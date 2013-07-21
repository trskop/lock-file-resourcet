{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Description:  Provide exclusive access to a resource using lock file.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP)
--
-- Provide exclusive access to a resource using lock file.
module System.IO.LockFile.Resource
    (
    -- * Locking primitives
      acquireLockFile
    , acquireLockFile_
    , acquireLockFile'

    -- * Configuration
    , LockingParameters(..)
    , RetryStrategy(..)

    -- * Exceptions
    , LockingException(..)
    )
    where

import Control.Applicative ((<$>))
import Control.Monad (void)

import Control.Monad.TaggedException
    ( Throws
#if MIN_VERSION_tagged_exception_core(1,1,0)
    , MonadExceptionUtilities
#endif
    , hide
    , liftT
    )
#if !MIN_VERSION_tagged_exception_core(1,1,0)
import Control.Monad.TaggedException.Utilities (MonadExceptionUtilities)
#endif

import Control.Monad.Trans.Resource
import System.IO.LockFile.Internal
    ( LockingException(..)
    , LockingParameters(..)
    , RetryStrategy(..)
    , lock
    , unlock
    )


acquireLockFile'
    :: (MonadExceptionUtilities m, MonadResource m)
    => LockingParameters
    -> FilePath
    -> m ReleaseKey
acquireLockFile' params lockFileName = fst <$> allocate lock' unlock'
  where
    lock' = hide $ lock params lockFileName
    unlock' = hide . unlock lockFileName

acquireLockFile
    :: (MonadExceptionUtilities m, MonadResource m)
    => LockingParameters
    -> FilePath
    -> Throws LockingException m ReleaseKey
acquireLockFile = (liftT .) . acquireLockFile'

acquireLockFile_
    :: (MonadExceptionUtilities m, MonadResource m)
    => LockingParameters
    -> FilePath
    -> Throws LockingException m ()
acquireLockFile_ = (void .) . acquireLockFile
