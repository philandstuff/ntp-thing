import           Control.Exception        (bracket)
import           Control.Monad            (liftM)
import           Data.Binary     (Binary, decode, get, put)
import           Data.Binary.Get (skip)
import           Data.Binary.Put (Put, putWord8, putWord16be, putWord32be, runPut)
import qualified Data.ByteString.Lazy  as BL
import           Data.Word        (Word32)
import           Network.BSD
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString.Lazy as NB
import           System.Posix.Env (getEnvDefault)

hostSocketAddress host port = do
  hostEntry <- getHostByName host
  return $ NS.SockAddrInet port (hostAddress hostEntry)

withUdpSocket :: NS.SockAddr -> (NS.Socket -> IO c) -> IO c
withUdpSocket addr =
  bracket
    (do socket <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        NS.connect socket addr
        return socket)
    NS.sClose

data Timestamp = Timestamp Word32 Word32 deriving Show

putTimestamp :: Timestamp -> Put
putTimestamp (Timestamp i f) = putWord32be i >> putWord32be f

getTimestamp = do
  i <- get
  f <- get
  return $ Timestamp i f

data Ntp2Packet =
  Ntp2SyncPacket { reference :: Timestamp,
                   originate :: Timestamp,
                   receive   :: Timestamp,
                   transmit  :: Timestamp}
  deriving Show

instance Binary Ntp2Packet where
  put = undefined
  get = do
    skip 16
    ref  <- getTimestamp
    orig <- getTimestamp
    rx   <- getTimestamp
    tx   <- getTimestamp
    return $ Ntp2SyncPacket ref orig rx tx

emptyPacket :: BL.ByteString
emptyPacket = runPut $
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
  recvPacket <- withUdpSocket addr
                (\ socket -> do
                    NB.send socket emptyPacket
                    NB.recv socket 1024)
  putStrLn $ show $ BL.unpack recvPacket
  putStrLn $ show recvPacket
  putStrLn $ show $ (decode recvPacket :: Ntp2Packet)
