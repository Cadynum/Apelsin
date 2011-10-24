module NumberSerializer where
import Data.Bits
import Network.Tremulous.SocketExtensions
import Network.Socket

intToHex :: (Integral i, Bits i) => Int -> i -> String
intToHex = go [] where
	go b 0 _   = b
	go b n int = go (toDigit (int .&. 0xF) : b) (n-1) (int `shiftR` 4)
	toDigit i | i <= 9    = conv (i + 0x30)
	          | otherwise = conv (i + 0x61-0xa)
	conv = toEnum . fromIntegral

hexToInt :: (Integral i, Bits i) => String -> i
hexToInt = go 0 where
	go b []     = b
	go b (x:xs) = go ((b `shiftL` 4) .|. toInt x) xs
	toInt c | c' <= 0x39 = c' - 0x30
	        | otherwise  = c' - (0x61-0xa)
	        where c' = conv c
	conv = fromIntegral . fromEnum


stringToSockAddr :: String -> String -> SockAddr
stringToSockAddr ip port = SockAddrInet (PortNum (htons (hexToInt port))) (htonl (hexToInt ip))
