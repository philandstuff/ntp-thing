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
              do putWord8 0x13 -- ntpv2, client
                 putWord8 0x10 -- stratum 16, unsynced
                 putWord8 16   -- min poll interval
                 putWord8 0    -- precision
                 putWord32be 0 -- sync distance
                 putWord32be 0 -- sync dispersion
                 putWord32be 0 -- ref id
                 putTimestamp epoch -- reference
                 putTimestamp epoch -- originate
                 putTimestamp epoch -- receive
                 putTimestamp epoch -- transmit

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
