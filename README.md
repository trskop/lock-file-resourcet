Lock File support for ResourceT
===============================


Description
-----------

Provide exclusive access to a resource using lock file, which are files whose
purpose is to signal by their presence that some resource is locked.


Dependencies
------------

Some dependencies aren't currently available on Hackage and have to be
installed manually:

* [`tagged-exception-core`](https://github.com/trskop/tagged-exception)
* [`lock-file`](https://github.com/trskop/lock-file)


Usage Example
-------------

Following example acquires lock file and then waits `1000000` micro seconds
before releasing it. Note also that it is possible to specify retry strategy.
Here we set it to `No` and therefore this code won't retry to acquire lock file
after first failure.

```Haskell
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
```

This command line example shows that trying to execute two instances of
`example` at the same time will result in failure of the second one.

```
$ ghc -Wall -package-conf=`pwd`/cabal-dev/packages-7.4.1.conf example.hs 
[1 of 1] Compiling Main             ( example.hs, example.o )
Linking example ...
$ ./example & ./example 
[1] 18018
Locking failed with: Unable to acquire lock file: "/var/run/lock/my-example-lock"
$ [1]+  Done                    ./example
```


Building options
----------------

* `-fpedantic` (disabled by default) --
  Pass additional warning flags including `-Werror` to GHC during compilation.
