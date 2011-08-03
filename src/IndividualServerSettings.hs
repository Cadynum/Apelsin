module IndividualServerSettings (
	ServerArg(..), ServerSettings
	, getSettings, putSettings, fromFile, toFile
) where
import Control.Applicative
import Network.Socket
import Data.Maybe
import Data.Map (Map)
import Data.ByteString.Internal
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Network.Tremulous.SocketExtensions
import Network.Tremulous.ByteStringUtils

import Constants
import Exception2


data ServerArg = ServerArg
	{ serverPass
	, serverRcon
	, serverName :: !String
	} deriving Show

type ServerSettings = Map SockAddr ServerArg

configFile :: IO FilePath
configFile = inConfDir "serversettings"

getSettings :: SockAddr -> ServerSettings -> ServerArg
getSettings = M.findWithDefault (ServerArg "" "" "")

putSettings :: SockAddr -> ServerArg -> ServerSettings -> ServerSettings
putSettings = M.insert 

fromFile :: IO ServerSettings
fromFile = handleIOWith M.empty $ parse <$> (B.readFile =<< configFile)

toFile :: ServerSettings -> IO Bool
toFile settings = tryIO $ do
	fx <- configFile
	B.writeFile fx $ B.unlines $ map f $ M.toList settings
	where f (addr, ServerArg a b c) = B.intercalate "\t" [putIPv4 addr, B.pack a, B.pack b, B.pack c]

parse :: ByteString -> ServerSettings
parse = M.fromList . mapMaybe (f . B.split '\t') . splitlines
	where
	f (addr:pass:rcon:name:_)
		| [i0, i1, i2, i3, p0, p1] <- map c2w (B.unpack addr)
		= Just (getIPv4 i0 i1 i2 i3 p0 p1, ServerArg (B.unpack pass) (B.unpack rcon) (B.unpack name))
	f _  = Nothing
