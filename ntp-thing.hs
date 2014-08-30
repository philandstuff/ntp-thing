import           Control.Exception        (bracket)
import           Control.Monad            (liftM, liftM2, liftM4)
import           Data.Binary     (Binary, decode, get, put)
import           Data.Binary.Get (Get, getWord8, skip)
import           Data.Binary.Put (Put, putWord8, putWord16be, putWord32be, runPut)
import           Data.Bits
import qualified Data.ByteString.Lazy  as BL
import           Data.Word        (Word8, Word32)
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

getTimestamp :: Get Timestamp
getTimestamp =
  liftM2 Timestamp get get

data LeapIndicator = None | AddSecondToday | DropSecondToday | Unknown
                   deriving (Show, Enum)

data Mode = Reserved | SymmActive | SymmPassive | Client | Server | Broadcast | NtpControl | PrivateUse
          deriving (Show, Enum)

getLeapVersionMode :: Get (LeapIndicator, Word8, Mode)
getLeapVersionMode = do
  word <- getWord8
  let li = toEnum $ fromIntegral $ word `shiftR` 6
  let version = (word `shiftR` 3) .&. 0x7
  let mode = toEnum $ fromIntegral $ word .&. 0x7
  return (li,version,mode)

data Ntp2Packet =
  Ntp2SyncPacket { li        :: LeapIndicator,
                   version   :: Word8,
                   mode      :: Mode,
                   stratum   :: Word8,
                   reference :: Timestamp,
                   originate :: Timestamp,
                   receive   :: Timestamp,
                   transmit  :: Timestamp}
  deriving Show

instance Binary Ntp2Packet where
  put = undefined
  get = do
    (li,version,mode) <- getLeapVersionMode
    let packetSoFar = Ntp2SyncPacket li version mode
    packetSoFar <- liftM packetSoFar getWord8
    skip 14
    liftM4 packetSoFar getTimestamp getTimestamp getTimestamp getTimestamp

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
