import           Network.BSD
import qualified Network.Socket as NS

hostSocketAddress host port = do
  hostEntry <- getHostByName host
  return $ NS.SockAddrInet port (hostAddress hostEntry)

sendMessage socketAddress message = do
  sendSocket <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
  NS.sendTo sendSocket message socketAddress


main = do
  addr <- hostSocketAddress "localhost" 1025
  sendMessage addr "hello"
