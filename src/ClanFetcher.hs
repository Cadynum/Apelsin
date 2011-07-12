module ClanFetcher( 
	Clan(..), TagExpr, matchTagExpr, prettyTagExpr, tagExprGet, getClanList, clanListFromCache
) where

import Prelude as P hiding (catch)
import Control.Exception
import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as L
import Data.Char (intToDigit)
import Data.Maybe
import Network.HTTP
import Network.URI
import Network.Tremulous.NameInsensitive
import TremFormatting

import Constants

data TagExpr =
	  TagPrefix !TI
	| TagSuffix !TI
	| TagInfix !TI
	| TagContained !TI !TI
	| TagError
	deriving Eq

instance Ord TagExpr where
	compare a b = compare (tagExprGet a) (tagExprGet b) where

tagExprGet :: TagExpr -> B.ByteString
tagExprGet x = case x of
	TagPrefix (TI _ v)	-> v
	TagSuffix (TI _ v)	-> v
	TagInfix (TI _ v)	-> v
	TagContained (TI _ v) _	-> v
	TagError		-> ""

data Clan = Clan {
	  clanid	:: !B.ByteString
	, name		:: !TI
	, website
	, irc		:: !B.ByteString
	, tagexpr	:: !TagExpr
	}

mkTagExpr :: B.ByteString -> TagExpr
mkTagExpr str
	| B.length str > 0 = case x of
	'<' -> TagPrefix (mk xs)
	'>' -> TagSuffix (mk xs)
	'^' -> TagInfix (mk xs)
	'%' -> let (a, b) = B.break (=='%') xs in TagContained (mk a) (mk (B.drop 1 b))
	_   -> TagError
	| otherwise = TagError
	where	(x, xs)	= (B.head str, B.tail str)

matchTagExpr :: TagExpr -> TI -> Bool
matchTagExpr expr raw = case expr of
	TagPrefix (TI _ xs)		-> xs `B.isPrefixOf` str
	TagSuffix (TI _ xs)		-> xs `B.isSuffixOf` str
	TagInfix (TI _ xs)		-> xs `B.isInfixOf` str
	TagContained (TI _ xs) (TI _ ys)-> xs `B.isPrefixOf` str && ys `B.isSuffixOf` str
	TagError		-> False
	where str = cleanedCase raw

prettyTagExpr :: TagExpr -> String
prettyTagExpr expr = case expr of
	TagPrefix bs	-> esc bs ++ wild
	TagSuffix bs	-> wild ++ esc bs
	TagInfix bs	-> wild ++ esc bs ++ wild
	TagContained a b-> esc a ++ wild ++ esc b
	TagError	-> "<span color=\"#BBB\">error</span>"
	where	wild = "<span color=\"#BBB\">*</span>"
		esc  = htmlEscape . B.unpack . original


rawToClan :: L.ByteString -> Maybe [Clan]
rawToClan = mapM (f . B.split '\t' . lazyToStrict) . P.filter (not . L.null) . L.split '\n' where 
	f [clanid, rname, _, website, irc, rexpr, _] =
		Just Clan	{ name = mkAlphaNum rname
				, tagexpr = mkTagExpr rexpr
				, .. }
	f _	= Nothing

getClanList :: String -> IO (Maybe [Clan])
getClanList url = do
	cont <- get url
	case cont of
		Right _ -> return Nothing
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
get url = case parseURI url of
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
