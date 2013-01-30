-- | Module: UdpLaunch
--   Author: Alexander Vershilov 
--   License: LGPL
module UdpLaunch
  where

import Control.Applicative
import Control.Exception
import Control.Monad (void)

import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString

waitUdp :: Int -> IO ()
waitUdp port = do
   bracket (do a <- head <$> getAddrInfo (Just defaultHints{ addrFlags=[AI_PASSIVE, AI_NUMERICHOST, AI_ALL, AI_NUMERICSERV]
                                                           , addrSocketType = Datagram
                                                           , addrFamily = AF_INET6
                                                           })
                                         Nothing
                                         (Just (show port))
               sock <- socket (addrFamily a) Datagram (addrProtocol a)
               setSocketOption sock ReuseAddr 1
               setSocketOption sock Broadcast 1
               bind sock (addrAddress a)
               return sock
               )
           (sClose)
           (void . flip recvFrom 16)

