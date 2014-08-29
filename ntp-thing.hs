import           Control.Exception        (bracket)
import           Control.Monad            (liftM)
import           Data.Binary.Put (Put, putWord32be, runPut)
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
              do putWord32be 0
                 putWord32be 0
                 putWord32be 0
                 putWord32be 0
                 putTimestamp $ epoch
                 putTimestamp $ epoch
                 putTimestamp $ epoch
                 putTimestamp $ epoch

epoch = Timestamp 0 0

main = do
  host <- getEnvDefault "HOST" "localhost"
  port <- liftM read $ getEnvDefault "PORT" "1025"
  addr <- hostSocketAddress host (toEnum port)
  withUdpSocket
    (\ socket ->
        NB.sendTo socket emptyPacket addr
    )
