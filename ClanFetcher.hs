module ClanFetcher( 
	Clan(..), getClanList, clanListFromCache
) where

import Tremulous.NameInsensitive

import Control.Exception
import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as L
import Data.Char (intToDigit)
import Prelude as P hiding (catch)
import Network.HTTP
import Network.URI
import Data.Maybe
import Constants
import GtkUtils

data Clan = Clan {
	  clanid	:: !B.ByteString
	, name
	, tag		:: !TI
	, website
	, irc		:: !B.ByteString
	}

rawToClan :: L.ByteString -> Maybe [Clan]
rawToClan = mapM (f . B.split '\t' . lazyToStrict) . P.filter (not . L.null) . L.split '\n' where 
	f [clanid, rname, rtag, website, irc]	= Just Clan {name = mkAlphaNum rname, tag = mk rtag,..}
	f _					= Nothing

getClanList :: String -> IO (Maybe [Clan])
getClanList url = do
	cont <- get url
	case cont of
		Right err -> do	gtkError $ "Failed retrieving new clanlist:\n" ++ err
				return Nothing
		Left raw -> do
			file	<- inCacheDir "clans"
			let clans = rawToClan raw
			when (isJust clans) $
				L.writeFile file raw

			return clans
			

clanListFromCache :: IO [Clan]
clanListFromCache = handle err $ do
	file	<- inCacheDir "clans"
	fromMaybe [] . rawToClan <$> L.readFile file
	where
	err (_::IOError) = return [] 
	

lazyToStrict :: L.ByteString -> B.ByteString		
lazyToStrict = B.concat . toChunks


-- I blame this on buggy Network.HTTP
get :: HStream ty => String -> IO (Either ty String)
get url = do
	case parseURI url of
		Nothing -> return $ Right "Error in URL"
		Just uri -> do
			resp <- catch (getRight `fmap` simpleHTTP (mkRequest GET uri)) err
			return $ case resp of
				Left msg				-> Right $ show msg
				Right (Response (2,0,0) _  _ body)	-> Left body
				Right (Response code reason _ _)	-> Right $ showRspCode code  ++ " " ++ reason
	where
	showRspCode (a,b,c) = P.map intToDigit [a,b,c]
	getRight (Right a) = Right a
	getRight (Left _) = error ""
	err :: IOException -> IO (Either String a )
	err = return . Left . show
