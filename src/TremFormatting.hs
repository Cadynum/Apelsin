module TremFormatting where
import Data.Array
import Data.Char
import Network.Tremulous.NameInsensitive

data TremFmt = TFColor !String | TFNone !String
	deriving (Show, Read)

type ColorArray = Array Int TremFmt

htmlEscape :: String -> String
htmlEscape = foldr f [] where
	f x xs = case x of 
		'<' -> "&lt;" ++ xs
		'>' -> "&gt;" ++ xs
		'&' -> "&amp;" ++ xs
		_   -> x : xs 
	
pangoColors :: ColorArray -> String -> String
pangoColors arr = f False where
	f n ('^':x:xs) | isAlphaNum x = case arr ! (a `mod` 8) of
		TFColor color	-> close n ++ "<span color=\"" ++ color ++ "\">" ++ f True xs
		TFNone	_	-> close n ++ f False xs
		where a = ord x - ord '0'
			  
			
	f n (x:xs)	= x:f n xs
	f n []		= close n
	
	close n = if n then "</span>" else ""

pangoPretty :: ColorArray-> TI -> String
pangoPretty arr = pangoColors arr . htmlEscape . unpackorig
