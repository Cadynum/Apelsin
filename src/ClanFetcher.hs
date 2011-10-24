module ClanFetcher(
	Clan(..), TagExpr, ClanID, matchTagExpr, prettyTagExpr, tagExprGet, getClanList, clanListFromCache
	, matchClanByID
) where

import Prelude as P
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as L
import Data.List as List
import Data.Maybe
import Data.Ord
import Network.Socket
import Network.HTTP
import Network.Stream
import Network.URI
import Network.Tremulous.NameInsensitive

import TremFormatting
import Constants
import Monad2

data TagExpr
	= TagPrefix !TI
	| TagSuffix !TI
	| TagInfix !TI
	| TagContained !TI !TI
	deriving Eq

instance Ord TagExpr where
	compare = comparing tagExprGet

tagExprGet :: TagExpr -> TI
tagExprGet x = case x of
	TagPrefix v		-> v
	TagSuffix v		-> v
	TagInfix v		-> v
	TagContained v _	-> v

type ClanID = Int
data Clan = Clan
	{ clanID	:: !ClanID
	, name		:: !TI
	, website
	, irc		:: !B.ByteString
	, tagexpr	:: !TagExpr
	, clanserver	:: !(Maybe SockAddr)
	}

matchClanByID :: [Clan] -> TI -> ClanID -> Bool
matchClanByID clans raw cid = case List.find (\x -> clanID x == cid) clans of
	Just a -> matchTagExpr (tagexpr a) raw
	Nothing -> False

mkTagExpr :: B.ByteString -> Maybe TagExpr
mkTagExpr str
	| Just (x, xs) <- B.uncons str = case x of
		'<' -> Just (TagPrefix (mk xs))
		'>' -> Just (TagSuffix (mk xs))
		'^' -> Just (TagInfix (mk xs))
		'%' -> let (a, b) = B.break (=='%') xs
				in Just (TagContained (mk a) (mk (B.drop 1 b)))
		_   -> Nothing
	| otherwise = Nothing

matchTagExpr :: TagExpr -> TI -> Bool
matchTagExpr expr raw = case expr of
	TagPrefix (TI _ xs)		-> xs `B.isPrefixOf` str
	TagSuffix (TI _ xs)		-> xs `B.isSuffixOf` str
	TagInfix (TI _ xs)		-> xs `B.isInfixOf` str
	TagContained (TI _ xs) (TI _ ys)-> xs `B.isPrefixOf` str && ys `B.isSuffixOf` str
	where str = cleanedCase raw

prettyTagExpr :: TagExpr -> B.ByteString
prettyTagExpr expr = case expr of
	TagPrefix bs	-> esc bs `B.append` wild
	TagSuffix bs	-> wild `B.append` esc bs
	TagInfix bs	-> B.concat [wild, esc bs, wild]
	TagContained a b-> B.concat [esc a, wild, esc b]
	where	wild = "<span color=\"#BBB\">*</span>"
		esc  = htmlEscapeBS . original


rawToClan :: L.ByteString -> IO (Maybe [Clan])
rawToClan = fmap sequence . mapM (f . B.split '\t' . lazyToStrict) . P.filter (not . L.null) . L.split '\n' where
	f [rclanID, rname, _, website, irc, rexpr, rserver]
		| Just tagexpr <- mkTagExpr rexpr
		, Just (clanID,_) <- B.readInt rclanID = do
			let	(ip, port1)	= B.break (==':') rserver
				port		= B.drop 1 port1
			clanserver <- if B.null ip || B.null port
				then return Nothing
				else getDNS (B.unpack ip) (B.unpack port)
			return $ Just Clan { name = mkAlphaNum rname, .. }
	f _	= return Nothing

getClanList :: String -> IO (Maybe [Clan])
getClanList url = do
	cont <- get url
	case cont of
		Nothing -> return Nothing
		Just raw -> do
			file	<- cacheFile
			clans	<- rawToClan raw
			when (isJust clans) $
				L.writeFile file raw
			return clans


clanListFromCache :: IO [Clan]
clanListFromCache = handle err $ do
	file <- cacheFile
	fromMaybe [] <$> (rawToClan =<< L.readFile file)
	where
	err (_ :: IOException) = return []


lazyToStrict :: L.ByteString -> B.ByteString
lazyToStrict = B.concat . toChunks

cacheFile :: IO FilePath
cacheFile = inCacheDir "clans"


get :: HStream ty => String -> IO (Maybe ty)
get url = case parseURI url of
	Nothing -> return Nothing
	Just uri -> do
		resp <- ex $ simpleHTTP (mkRequest GET uri)
		return $ case resp of
			Right (Response (2,0,0) _  _ body)	-> Just body
			_					-> Nothing
	-- It doesn't catch everything apparently
	where ex = handle (\(_ :: IOException) -> return (Left ErrorReset))
