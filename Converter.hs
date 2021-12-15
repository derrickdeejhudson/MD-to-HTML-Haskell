import Data.Char
import Data.List
import System.IO

data Token   = H1 String | H2 String | H3 String 
             | BlockQuote String | Paragraph String | Blank | Hr
             | Ol Integer String| Ul Integer String 
             | Code String | Link String | Image String
             | Html [Token] | Body [Token]
    deriving (Show, Eq)

countTabs :: String -> Integer
countTabs ""     = 0
countTabs (x:xs) | x == '\t' = 1 + countTabs xs
                 | True      = countTabs xs

subTab :: String -> String
subTab ""        = ""
subTab ('\t':xs) = subTab xs
subTab str       = str

lexer :: [String] -> [(Integer, Token)]
lexer []     = []
lexer (x:xs) = [(countTabs x, classify (subTab $ parser' x) (countTabs x))] ++ (lexer xs)

isValidLink :: [Integer] -> Bool
isValidLink []       = True
isValidLink [x]      = True
isValidLink (x:y:xs) = (x < y) && isValidLink(y:xs)

isValidImage :: [Integer] -> Bool
isValidImage []       = True
isValidImage [x]      = True
isValidImage (x:y:xs) = (x < y) && isValidImage(y:xs)

getLink :: String -> Bool -> String
getLink "" found             = ""
getLink (']':'(':x:xs) found = x : getLink xs True
getLink (')':xs) found       = ""
getLink (x:xs) found         | (found == True) = x : getLink xs found
                             |  True           = getLink xs found

getTitle :: String -> Bool -> String
getTitle "" found         = ""
getTitle ('[':x:xs) found = x : getTitle xs True
getTitle (']':xs) found   = ""
getTitle (x:xs) found     | found == True = x : getTitle xs found
                          | True          = getTitle xs found

endOfTerm :: String -> Bool -> String
endOfTerm "" found         = ""
endOfTerm (')':x:xs) found = x : endOfTerm xs True
endOfTerm (x:xs) found     | found == True = x : endOfTerm xs found
                           | True          = endOfTerm xs found

classify :: String -> Integer -> Token
classify [] depth                  = Blank
classify ('#':'#':'#':' ':x) depth = H3 x
classify ('#':'#':' ':x) depth     = H2 x
classify ('#':' ':x) depth         = H1 x
classify ('-':'-':'-':x) depth     = Hr
classify ('>':' ':x) depth         = BlockQuote x
classify ('`':x) depth             = Code x
classify ('-':' ':x) depth         = Ul depth (parser' x)
classify (x:'.':' ':xs) depth      = if (isNumber(x)) then Ol depth (parser' xs) else Paragraph (x:'.':xs)
classify str depth                 = Paragraph str

linkParser :: String -> String
linkParser ""     = ""
linkParser (x:xs) = if ((length $ linkHelper (x:xs) 0) == 3 && (isValidLink $ linkHelper (x:xs) 0) && (head $ linkHelper (x:xs) 0) == 0)
                    then "<a href=\"" ++ getLink (x:xs) False ++ "\">" ++ getTitle (x:xs) False ++ "</a>" ++ linkParser (endOfTerm xs False)
                    else x : linkParser xs

imageParser :: String -> String
imageParser "" = ""
imageParser (x:xs) = if ((length $ imageHelper (x:xs) 0) == 3 && (isValidImage $ imageHelper (x:xs) 0) && (head $ imageHelper (x:xs) 0) == 0)
                     then "<img src=\"" ++ getLink (x:xs) False ++ "\" alt=\"" ++ getTitle (x:xs) False ++ "\"></img>" ++ imageParser (endOfTerm xs False)
                     else x : imageParser xs

italicParser :: String -> String
italicParser str = if ((length (italicHelper str 0)) <= 1) 
                   then str
                   else if (odd $ length (italicHelper str 0)) 
                   then italic str 0 (closeTag (init (italicHelper str 0)) 0)
                   else italic str 0 (closeTag (italicHelper str 0) 0)

boldParser :: String -> String
boldParser str = if ((length (boldHelper str 0)) <= 1) 
                 then str
                 else if (odd $ length (boldHelper str 0)) 
                 then bold str 0 (closeTag (init (boldHelper str 0)) 0)
                 else bold str 0 (closeTag (boldHelper str 0) 0)

closeTag :: [Integer] -> Integer -> [Integer]
closeTag [] c     = []
closeTag (x:xs) c | odd c = (x * (-1)) : (closeTag xs (c + 1))
                  | True  = x : (closeTag xs (c + 1))

linkHelper :: String -> Integer -> [Integer]
linkHelper "" n           = []
linkHelper ('[':xs) n     = n : linkHelper xs (n + 1)
linkHelper (']':'(':xs) n = n : linkHelper xs (n + 1)
linkHelper (')':xs) n     = [n]
linkHelper (']':xs) n     = n : linkHelper xs (n + 1)
linkHelper (x:xs) n       = linkHelper xs (n + 1)

imageHelper :: String -> Integer -> [Integer]
imageHelper "" n           = []
imageHelper ('!':'[':xs) n = n : imageHelper xs (n + 1)
imageHelper (']':'(':xs) n = n : imageHelper xs (n + 1)
imageHelper (')':xs) n     = [n]
imageHelper (']':xs) n     = n : imageHelper xs (n + 1)
imageHelper (x:xs) n       = imageHelper xs (n + 1)

italicHelper :: String -> Integer -> [Integer]
italicHelper "" n      = []
italicHelper ('*':x) n = n : (italicHelper x (n + 1))
italicHelper (x:xs) n  = italicHelper xs (n + 1)

boldHelper :: String -> Integer -> [Integer]
boldHelper "" n          = []
boldHelper ('*':'*':x) n = n : (boldHelper x (n + 1))
boldHelper (x:xs) n      = boldHelper xs (n + 1)

italic :: String -> Integer -> [Integer] -> String
italic "" n list       = ""
italic ('*':xs) n list | ((n * (-1)) `elem` list && n /= 0) = "</em>" ++ italic xs (n + 1) list
                       | (n `elem` list) =  "<em>" ++ italic xs (n + 1) list
italic (x:xs) n list   = x : italic xs (n + 1) list

bold :: String -> Integer -> [Integer] -> String
bold "" n list           = ""
bold ('*':'*':xs) n list | ((n * (-1)) `elem` list && n /= 0) = "</strong>" ++ bold xs (n + 1) list
                         | (n `elem` list) =  "<strong>" ++ bold xs (n + 1) list
bold (x:xs) n list       = x : bold xs (n + 1) list

blankFilter :: [(Integer, Token)] -> [(Integer, Token)]
blankFilter []                  = []
blankFilter ((depth, Blank):xs) = (0, Blank) : blankFilter xs
blankFilter (x:xs)              = x : blankFilter xs

dfs :: [(Integer, Token)] -> Integer -> [Token]
dfs [] depth = []
dfs ((n, H1 str):xs) depth           = if (depth == n) then H1 str : dfs xs depth else []
dfs ((n, H2 str):xs) depth           = if (depth == n) then H2 str : dfs xs depth else []
dfs ((n, H3 str):xs) depth           = if (depth == n) then H3 str : dfs xs depth else []
dfs ((n, BlockQuote str):xs) depth   = if (depth == n) then BlockQuote str : dfs xs depth else []
dfs ((n, Code str):xs) depth         = if (depth == n) then Code str : dfs xs depth else []
dfs ((n, Paragraph str):xs) depth    = if (depth == n) then Paragraph str : dfs xs depth else []
dfs ((n, Blank):xs) depth            = if (depth == n) then  Blank : dfs xs depth else []
dfs ((n, Hr):xs) depth               = if (depth == n) then  Hr : dfs xs depth else []
dfs ((n, Ol x str):xs) depth         = if (depth == n) then Ol x str : dfs xs depth else []
dfs ((n, Ul x str):xs) depth         = if (depth == n) then Ul x str : dfs xs depth else []

parser :: [(Integer, Token)] -> Token
parser [] = Blank
parser ((depth, Html list):xs) = Html (parser xs : list)
parser ((depth, Body list):xs) = Body ((dfs xs depth) ++ list)

parser' :: String -> String
parser' ""  = ""
parser' str = linkParser (imageParser (italicParser (boldParser str)))

html :: Token -> [(Integer, Token)] -> String
html Blank x         = ""
html ((Html list)) x = "<html>\n\n\t<head>\n" ++ "\n\t</head>\n\n" ++ (htmlGenerator list x) ++ "\n</html>"

htmlGenerator :: [Token] -> [(Integer, Token)] -> String
htmlGenerator [] x                     = ""
htmlGenerator ((Body list):xs) x       =  "\t<body>\n" ++ (htmlGenerator list x) ++ "\t<body>\n"
htmlGenerator ((H1 str):xs) x          = "\t\t<h1>" ++ str ++ "</h1>\n" ++ (htmlGenerator xs x)
htmlGenerator ((H2 str):xs) x          = "\t\t<h2>" ++ str ++ "</h2>\n" ++ (htmlGenerator xs x)
htmlGenerator ((H3 str):xs) x          = "\t\t<h3>" ++ str ++ "</h3>\n" ++ (htmlGenerator xs x)
htmlGenerator ((Hr):xs) x              = "\t\t<hr>\n" ++ (htmlGenerator xs x)
htmlGenerator ((Blank):xs) x           = "\n" ++ (htmlGenerator xs x)
htmlGenerator ((Paragraph str):xs) x   = "\t\t<p>" ++ str ++ "</p>\n" ++ (htmlGenerator xs x)
htmlGenerator ((BlockQuote str):xs) x  = "\t\t<blockquote>" ++ str ++ "</blockquote>\n" ++ (htmlGenerator xs x)
htmlGenerator ((Code str):xs) x        = "\t\t<code>" ++ str ++ "</code>\n" ++ (htmlGenerator xs x)
htmlGenerator ((Ul depth str):xs) x    = "\t\t<li>" ++ str ++ "</li>\n" ++ (htmlGenerator xs x)
htmlGenerator ((Ol depth str):xs) x    = "\t\t<li>" ++ str ++ "</li>\n" ++ (htmlGenerator xs x)

main :: IO ()
main = do
    putStrLn "Enter a file to convert"
    fileName <- getLine
    input <- readFile fileName
    let line = lines input
    let lexed = [(0, Html [])] ++ [(0, Body [])] ++ lexer line
    let parsed =  parser $ blankFilter $ lexed
    let makehtml = html parsed lexed
    writeFile "converted.html" makehtml
