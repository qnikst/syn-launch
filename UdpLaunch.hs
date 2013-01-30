-- | Module: UdpLaunch
--   Author: Alexander Vershilov 
--   License: LGPL
module UdpLaunch
  where

import Control.Exception

import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString

waitUdp :: Int -> IO ()
waitUdp port = do
   bracket (socket AF_INET Datagram defaultProtocol)
           (sClose)
           (\sock -> do setSocketOption sock ReuseAddr 1
                        setSocketOption sock Broadcast 1
                        bind sock $! SockAddrInet (fromIntegral port) iNADDR_ANY
                        _ <- recvFrom sock 16
                        return ())

