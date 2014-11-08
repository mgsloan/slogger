Here's a basic example of using slogger:

```haskell
import Control.Concurrent.Lifted
import Prelude hiding (log)
import Slogger

main :: IO ()
main = $runSloggerT $ do
  let x = 1
  $log "Hello, here's a number: " x
  fork $ $logNest "Forked process 1" $ do
    threadDelay (1000 * 1000)
    $log "After one second"
  fork $ $logNest "Forked process 2" $ do
    threadDelay (2000 * 1000)
    $log "After two seconds"
```



```haskell
  $logNest "Program running" $ do
    (x :: Int) <- readLn
    y <- $logRun 'helper x
    $log ""
```


$logFork
