module TremFormatting where
import Data.Array
import Data.Char
import Network.Tremulous.NameInsensitive
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

data TremFmt = TFColor !String | TFNone !String
	deriving (Show, Read)

type ColorArray = Array Int TremFmt

htmlEscapeBS :: ByteString -> ByteString
htmlEscapeBS = B.concat . B.foldr f [] where
	f x xs = case x of
		'<' -> "&lt;" : xs
		'>' -> "&gt;" : xs
		'&' -> "&amp;" : xs
		_   -> B.singleton x : xs

pangoPrettyBS :: ColorArray -> TI -> ByteString
pangoPrettyBS arr = B.pack . pangoColors arr . B.unpack . original

pangoPretty :: ColorArray-> TI -> String
pangoPretty arr = pangoColors arr . B.unpack . original

pangoColors :: ColorArray -> String -> String
pangoColors arr = f False where
	--Replace with colors
	f n ('^':x:xs) | isAlphaNum x = case arr ! (a `mod` 8) of
		TFColor color	-> close n ++ "<span color=\"" ++ color ++ "\">" ++ f True xs
		TFNone	_	-> close n ++ f False xs
		where a = ord x - ord '0'
	--Escape pango
	f n (x:xs) = case x of
		'<' -> "&lt;" ++ f n xs
		'>' -> "&gt;" ++ f n xs
		'&' -> "&amp;" ++ f n xs
		_   -> x : f n xs

	f n []		= close n

	close n = if n then "</span>" else ""


