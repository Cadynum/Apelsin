module TremFormatting where
import Helpers
import Data.Array
import Tremulous.NameInsensitive

--unfuckName :: Array Int TremFmt -> String -> String
unfuckName arr = pangoColors arr . htmlEscape . stripw . filter isPrint . unpackorig

boxify :: String -> String
boxify = map (\a -> if isPrint a then a else '☐')

htmlEscape :: String -> String
htmlEscape = foldr f [] where
	f x xs = case x of 
		'<' -> "&lt;" ++ xs
		'>' -> "&gt;" ++ xs
		'&' -> "&amp;" ++ xs
		_   -> x : xs 

data TremFmt = TFColor !String | TFNone
	deriving (Show, Read)
--pangoCo :: Array Int Fmt2 -> String -> String
pangoCo arr = f False where
	f n ('^':x:xs) | isAlphaNum x = case arr ! (a `mod` 8) of
		TFColor color	-> close n ++ "<span color=\"" ++ color ++ "\">" ++ f True xs
		TFNone		-> close n ++ f False xs
		where a = ord x - ord '0'
			  
			
	f n (x:xs)	= x:f n xs
	f n []		= close n
	
	close n = if n then "</span>" else ""

pangoColors = pangoCo

--pangoPretty :: Array Int TremFmt -> String -> String
pangoPretty arr = pangoColors arr . htmlEscape . unpackorig
