import           Control.Exception        (bracket)
import           Control.Monad            (liftM)
import           Data.Binary.Put (Put, putWord8, putWord16be, putWord32be, runPut)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Data.Word        (Word32)
import           Network.BSD
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NB
import           System.Posix.Env (getEnvDefault)

hostSocketAddress host port = do
  hostEntry <- getHostByName host
  return $ NS.SockAddrInet port (hostAddress hostEntry)

withUdpSocket :: (NS.Socket -> IO c) -> IO c
withUdpSocket =
  bracket
    (NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol)
    NS.sClose

data Timestamp = Timestamp Word32 Word32

putTimestamp :: Timestamp -> Put
putTimestamp (Timestamp i f) = putWord32be i >> putWord32be f

emptyPacket :: B.ByteString
emptyPacket = B.concat $
              BL.toChunks $
              runPut $
              do putWord8 0x16 -- ntpv2, control message
                 putWord8 0x01 -- "read status" operation
                 putWord16be 1 -- sequence number
                 putWord16be 0 -- "status"
                 putWord16be 0 -- "association id"
                 putWord16be 0 -- "offset"
                 putWord16be 0 -- "count"

epoch = Timestamp 0 0

main = do
  host <- getEnvDefault "HOST" "localhost"
  port <- liftM read $ getEnvDefault "PORT" "123"
  addr <- hostSocketAddress host (toEnum port)
  (recvPacket, recvAddr) <- withUdpSocket
                            (\ socket -> do
                                NB.sendTo socket emptyPacket addr
                                NB.recvFrom socket 1024
                            )
  putStrLn $ show $ B.unpack recvPacket
  putStrLn $ show recvPacket
