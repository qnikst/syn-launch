module NetSync
  where

import Network.Discovery

runDiscovery ip mask port = do
  let event b s = undefined
  host <- mkBroadcast ip mask
  -- TODO start TCP Server
  forkIO $ do
      forkIO $ discovery (DiscoveryOneWay (secondsToInterval 1) (return "myname"))
                         (event)
                         mask
                         port
      result <- atomically $ readTVar result
      case result of
        Nothing -> return ()
        Just xs  -> undefined -- RUN TCP Synchronization
  -- LISTEN TCP
  -- run synchronization and execute command
  
