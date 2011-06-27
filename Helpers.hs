module Helpers (
	module Data.Char
	, getDNS
	, strict, split, splitlines, breakDrop
	, firstWord, stripw
	, fromNull, maybeL
	, replace, dropWhileRev
	, (//), (%), atLeastLen
	, intmean, mread, lookupDelete
	, stripContainer, stripPrefixWith, stripSuffix, stripInfix
	, capitalize, formatTime, getMicroTime
	, liftMS, whileTrue
) where
import Data.Char
import Prelude hiding (foldr, foldl, foldr1, foldl1)
import Control.Monad hiding (forM_, mapM_, msum, sequence_)
import Data.Foldable
import Data.List (stripPrefix)
import Control.DeepSeq
import System.Time
import Control.Applicative
import Network.Socket
import Control.Exception

getDNS :: String -> String -> IO (Maybe SockAddr)
getDNS host port = handle (\(_::IOException) -> return Nothing) $ do
	AddrInfo _ _ _ _ addr _ <- Prelude.head `liftM` getAddrInfo Nothing (Just host) (Just port)
	return $ Just addr
	
strict :: NFData a => a -> a
strict x = rnf x `seq` x

stripw :: String -> String
stripw = dropWhileRev isSpace . dropWhile isSpace

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split func s = case dropWhile func s of
	[] -> []
	s' -> w : split func s''
		where (w, s'') = break func s'

breakDrop :: (a -> Bool) -> [a] -> ([a], [a])
breakDrop f xs = let (a, b) = break f xs
	in (a, dropWhile f b)

splitlines :: String -> [String]
splitlines = split (\a -> a == '\n' || a == '\r')

firstWord :: String -> String
firstWord = takeWhile (not . isSpace)

atLeastLen :: Integral t => t -> [a] -> Bool
atLeastLen 0 _		= True
atLeastLen _ []		= False
atLeastLen n (_:xs)	= atLeastLen (n-1) xs

fromNull :: [a] -> [a] -> [a]
fromNull x []	= x
fromNull _ x	= x

maybeL :: f -> (a -> f) -> [a] -> f
maybeL x _ []		= x
maybeL _ f (xs:_)	= f xs

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace s r = rep where
	rep []		= []
	rep xx@(x:xs)	= case stripPrefix s xx of
				Nothing	-> x : rep xs
				Just a	-> r ++ rep a

(//), (%) :: Integral a => a -> a -> a
(//) = div
(%) = mod

intmean :: (Integral i, Foldable f) => f i -> i
intmean l = if len == 0 then 0 else lsum // len where
	(len, lsum) = foldl' (\(!n, !s) elm -> (n+1, s+elm)) (0, 0) l

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []


mread :: (Read a) => String -> Maybe a
mread x = case reads x of
	[(a, _)]	-> Just a
	_		-> Nothing

lookupDelete :: (Eq k) => k -> [(k, v)] -> (Maybe v, [(k, v)])
lookupDelete key = roll where
	roll []			= (Nothing, [])
	roll (x@(a, b):xs)
		| key == a	= (Just b, xs)
		| otherwise	= let (may, xs') = roll xs in (may, x:xs')

stripPrefixWith :: (Eq t) => (t -> t) -> [t] -> [t] -> Maybe [t]
stripPrefixWith	f	(x:xs)	(y:ys)
	| f x == f y			= stripPrefixWith f xs ys
stripPrefixWith	_	[]	y	= Just y
stripPrefixWith	_	_	_	= Nothing

stripContainer :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
stripContainer x y l = stripPrefix x l >>= stripSuffix y

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix _	[]	= Nothing
stripSuffix p	xss@(x:xs)
	| p == xss	= Just []
	| otherwise	= (x:) `liftM` stripSuffix p xs

stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix _	[]	= Nothing
stripInfix p	xss@(x:xs) = case stripPrefix p xss of
	Nothing	-> x `f` stripInfix p xs
	Just a	-> Just ([], a)
	where	f a (Just (as, bs))	= Just (a:as, bs)
		f _ Nothing		= Nothing


capitalize :: String -> String
capitalize = unwords . map f . words
	where	f []		= []
		f (x:xs)	= toUpper x : fmap toLower xs


formatTime :: (Integral i) => i -> String
formatTime s = f day "day" ++ ", " ++ f hour "hour" ++ ", " ++ f min' "minute" ++  " and " ++ f sec "second"
	where
	sec	= s % 60
	min'	= (s // 60) % 60
	hour	= (s // (60*60)) % 24
	day	= (s // (60*60*24))

	f val str	= show val ++ ' ':str ++ (if val == 1 then "" else "s")


liftMS :: (Monad m) => (a -> b) -> (m a) -> (m b)
liftMS f v = (\x -> return $! f x) =<< v

getMicroTime :: IO Integer
getMicroTime = let f (TOD s p) = s*1000000 + p `div` 1000000 in f <$> getClockTime

whileTrue :: Monad m => m Bool -> m ()
whileTrue f = do
	t <- f
	if t 
	then whileTrue f
	else return ()
	
