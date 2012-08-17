module NumberSerializer where
import Data.Bits
import Network.Tremulous.SocketExtensions
import Network.Socket
import Data.Foldable

intToHex :: (Integral i, Bits i) => Int -> i -> String
intToHex = go [] where
	go b 0 _   = b
	go b n int = go (toDigit (int .&. 0xF) : b) (n-1) (int `shiftR` 4)
	toDigit i | i <= 9    = conv (i + 0x30)
	          | otherwise = conv (i + 0x61-0xa)
	conv = toEnum . fromIntegral

hexToInt :: (Integral i, Bits i) => String -> i
hexToInt = foldl' (\acc x -> (acc `shiftL` 4) .|. toInt x) 0
	where
	toInt c | c' <= 0x39 = c' - 0x30
	        | otherwise  = c' - (0x61-0xa)
	        where c' = conv c
	conv = fromIntegral . fromEnum


stringToSockAddr :: String -> String -> SockAddr
stringToSockAddr ip port = SockAddrInet
	(PortNum (htons (hexToInt port)))
	(htonl (hexToInt ip))
