-- |
-- Module:       $HEADER$
-- Description:  Provide exclusive access to a resource using lock file.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Provide exclusive access to a resource using lock file.
module System.IO.LockFile.Resource
    (
    -- * Locking primitives
      acquireLockFile

    -- * Configuration
    , LockingParameters(..)
    , RetryStrategy(..)

    -- * Exceptions
    , LockingException(..)
    )
    where

import Control.Applicative ((<$>))

import Control.Monad.TaggedException (hide)

import Control.Monad.Trans.Resource
    ( MonadResource(liftResourceT)
    , ReleaseKey
    , allocate
    )
import System.IO.LockFile.Internal
    ( LockingException(..)
    , LockingParameters(..)
    , RetryStrategy(..)
    , lock
    , unlock
    )


acquireLockFile
    :: MonadResource m
    => LockingParameters
    -> FilePath
    -> m ReleaseKey
acquireLockFile params lockFileName =
    liftResourceT $ fst <$> allocate lock' unlock'
  where
    lock' = hide $ lock params lockFileName
    unlock' = hide . unlock lockFileName
