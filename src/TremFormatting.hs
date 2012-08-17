module TremFormatting where
import Data.Array
import Data.Char
import Data.String
import Network.Tremulous.NameInsensitive
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

data TremFmt = TremFmt {active :: !Bool, color :: !String}
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
pangoColors arr = go False where
	--Replace with colors
	go n ('^':x:xs) | isAlphaNum x = close n ++ middle ++ go active xs
		where
		TremFmt {..} = arr ! ((ord x - ord '0') `rem` 8)
		middle	| active	= "<span color=\"" ++ color ++ "\">"
				| otherwise = ""

	-- Escape pango
	go n (x:xs) = case x of
		'<' -> "&lt;" ++ go n xs
		'>' -> "&gt;" ++ go n xs
		'&' -> "&amp;" ++ go n xs
		_   -> x : go n xs

	go n []		= close n

	close n = if n then "</span>" else ""

	-- Protocol version
protoToAbbr, protoToFull :: IsString s => Int ->  s
protoToAbbr x = case x of
	69 -> "1.1"
	70 -> "gpp"
	86 -> "unv"
	_  -> "?"

protoToFull x = case x of
	69 -> "Tremulous 1.1"
	70 -> "Tremulous GPP"
	86 -> "Unvanquished"
	_  -> "<Unknown>"