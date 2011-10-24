module IndividualServerSettings (
	ServerArg(..), ServerSettings
	, getSettings, putSettings, fromFile, toFile
) where
import Control.DeepSeq
import Network.Socket
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Network.Tremulous.SocketExtensions
import NumberSerializer

import Constants
import Exception2
import List2


data ServerArg = ServerArg
	{ serverPass
	, serverRcon
	, serverName		:: !String
	, serverFavorite	:: !Bool
	}

instance NFData ServerArg where
	rnf (ServerArg a b c _) = rnf a `seq` rnf b `seq` rnf c

type ServerSettings = Map SockAddr ServerArg

configFile :: IO FilePath
configFile = inConfDir "serversettings"

getSettings :: SockAddr -> ServerSettings -> ServerArg
getSettings = M.findWithDefault (ServerArg "" "" "" False)

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
	where f ((SockAddrInet (PortNum port) ip), ServerArg a b c d) = intercalate "\t"
		[ intToHex 8 (ntohl ip)
		, intToHex 4 (ntohs port)
		, a, b, c
		, if d then "f" else "" ]
	      f _ = ""

parse :: String -> ServerSettings
parse = M.fromList . mapMaybe (f . split '\t') . lines
	where
	f (ip:port:pass:rcon:name:opts:_) = Just (addr, ServerArg pass rcon name fav)
		where addr = SockAddrInet (PortNum (htons (hexToInt port))) (htonl (hexToInt ip))
		      fav  = 'f' `elem` opts
	f _  = Nothing
