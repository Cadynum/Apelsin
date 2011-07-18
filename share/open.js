var s		= WSH.CreateObject("WScript.Shell")
var url		= WSH.Arguments(0)
var valid	= "http://"
var valid2	= "https://"

if (isPrefix(valid, url) || isPrefix(valid2, url)) {
	s.Run(url)
}

function isPrefix (m, str) {
	return str.slice(0, m.length) == m
}
