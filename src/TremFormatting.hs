module TremFormatting where
import Data.Array
import Data.Char
import Network.Tremulous.NameInsensitive
import List2

data TremFmt = TFColor !String | TFNone
	deriving (Show, Read)

type ColorArray = Array Int TremFmt

unfuckName :: ColorArray -> TI -> String
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


	
pangoColors :: ColorArray -> String -> String
pangoColors arr = f False where
	f n ('^':x:xs) | isAlphaNum x = case arr ! (a `mod` 8) of
		TFColor color	-> close n ++ "<span color=\"" ++ color ++ "\">" ++ f True xs
		TFNone		-> close n ++ f False xs
		where a = ord x - ord '0'
			  
			
	f n (x:xs)	= x:f n xs
	f n []		= close n
	
	close n = if n then "</span>" else ""

pangoPretty :: ColorArray-> TI -> String
pangoPretty arr = pangoColors arr . htmlEscape . unpackorig
