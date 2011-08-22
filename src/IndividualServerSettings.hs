module IndividualServerSettings (
	ServerArg(..), ServerSettings
	, getSettings, putSettings, fromFile, toFile
) where
import Foreign
import Control.DeepSeq
import Network.Socket
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Network.Tremulous.SocketExtensions

import Constants
import Exception2
import List2


data ServerArg = ServerArg
	{ serverPass
	, serverRcon
	, serverName :: !String
	}

instance NFData ServerArg where
	rnf (ServerArg a b c) = rnf a `seq` rnf b `seq` rnf c

type ServerSettings = Map SockAddr ServerArg

configFile :: IO FilePath
configFile = inConfDir "serversettings"

getSettings :: SockAddr -> ServerSettings -> ServerArg
getSettings = M.findWithDefault (ServerArg "" "" "")

putSettings :: SockAddr -> ServerArg -> ServerSettings -> ServerSettings
putSettings = M.insert

fromFile :: IO ServerSettings
fromFile = handleIOWith M.empty $ do
	cont <- readFile =<< configFile
	return  $! strict (parse cont)
	where strict a = deepseq a a


toFile :: ServerSettings -> IO Bool
toFile settings = tryIO $ do
	fx <- configFile
	writeFile fx $ unlines $ map f $ M.toList settings
	where f ((SockAddrInet (PortNum port) ip), ServerArg a b c) = intercalate "\t" [intToHex 8 (ntohl ip), intToHex 4 (ntohs port), a, b, c]
	      f _ = ""

parse :: String -> ServerSettings
parse = M.fromList . mapMaybe (f . split (=='\t')) . lines
	where
	f (ip:port:pass:rcon:name:_) = Just (addr, ServerArg pass rcon name)
		where addr = SockAddrInet (PortNum (htons (hexToInt port))) (htonl (hexToInt ip))
	f _  = Nothing


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
