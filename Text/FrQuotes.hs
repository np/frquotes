-- The syntax is in the README.md file.
module Text.FrQuotes (frQuotes) where
import Data.Char (isLetter)
{-
import Test.QuickCheck

breakWithSpec :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
breakWithSpec p k xs = takeWhile p xs ++ k (dropWhile p xs)

breakWith_prop x y xs = breakWithSpec p k xs == breakWith p k xs
  where p = (==x)
        k = map (+y)
-}

--breakWith :: (a -> Bool) -> (a -> [a] -> [a]) -> [a] -> [a]

breakWith :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
breakWith _ k []
  = k []
breakWith p k (x:xs)
  | p x        = x : breakWith p k xs
  | otherwise  = k (x:xs)

qq67 :: String
-- GHC7
qq67 = ""
-- GHC6
-- qq67 = "$"

frTop, openFrQQ', closeFrQQ', openFrQQ, closeFrQQ,
  openBr, closeBr :: String
frTop = "(frTop ("
openFrQQ'  = "[" ++ qq67 ++ "frQQ|"
closeFrQQ' = "|]"
openFrQQ  = frTop ++ openFrQQ'
closeFrQQ = closeFrQQ' ++ "))"
-- one could generate a list and mconcat it instead of these mappend
openBr    = closeFrQQ' ++ " `mappend` frAntiq ("
closeBr   = ") `mappend` " ++ openFrQQ'

-- Substitutes UTF8 french quotes «...» for (frTop [frQQ|...|])
-- Antiquotations are supported via braces {...} and are
-- substituted for frAntiq, blocks are catenated with mappend.
frQuotes :: String -> String
frQuotes = h
        -- All these functions follow the same style,
        -- the first argument 'k' is the continuation, i.e. what to do next.
        -- Each function search for a different closing token, if the closing
        -- token is found the continuation is called with the remaining char
        -- stream, if some opening token is found the continuation is stacked
        -- as the continuation of a new function call that have to find this
        -- new token first.

        -- haskell context
        -- the h function don't needs a continuation parameter
  where h ""                   = ""
        h ('«':'{':xs) | noesc xs = frTop ++ "( frAntiq (" ++ bOq "" ((closeBr++) . f ((closeFrQQ++) . (')':) . h)) xs -- avoids an empty [frQQ||]
        h ('«':xs)             = openFrQQ ++ f ((closeFrQQ++) . h) xs
        h ('{':'-':xs)         = "{-" ++ c (("-}"++) . h) xs
        h ('"':xs)             = '"' : s (('"':) . h) xs
        h ('\'':xs)            = '\'' : a h xs
        h ('[':'$':xs)         = '[' : hOq "" h xs
        h ('[':xs)             = '[' : hOq "" h xs
        h ('-':'-':xs)         = "--" ++ mc h xs
        h (x:'-':'-':xs)       | x == ':' || isAscSymb x = x : '-' : '-' : breakWith (=='-') h xs
        h (x:xs)               = x : h xs

        noesc ('«':'}':_) = False
        noesc ('»':'}':_) = False
        noesc _           = True

        isAscSymb = (`elem` "!#$%&*+./<=>?@\\^|~")
        isSymb x = x == '-' || x == ':' || isAscSymb x

        -- maybe a one line comment
        mc k ""         = k ""
        mc k ('-':xs)   = '-' : mc k xs
        mc k ('\n':xs)  = '\n' : k xs
        mc k (x:xs) | isAscSymb x  = x : breakWith isSymb k xs
                    | otherwise    = x : breakWith (/='\n') k xs

        -- french quotes context
        f _ ""                 = error "unterminated french quotes (expecting `»')"
        f _ ('}':_)            = error "unexpected closing brace `}'"
        f k ('{':'«':'}':xs)   = '«' : f k xs
        f k ('{':'»':'}':xs)   = '»' : f k xs
        f k ('«':'}':'»':xs)   = '}' : f k xs
        f k ('«':'{':'»':xs)   = '{' : f k xs
        f k ('»':xs)           = k xs
        f k ('{':xs)           = openBr ++ bOq "" ((closeBr++) . f k) xs
        f k (x:xs)             = x : f k xs

        qName "" = "defaultQQ"
        qName xs = reverse xs

        -- brace hole OR quasi-quotation
        bOq qn k ""            = k (reverse qn)
        bOq qn k ('|':xs)      = "[" ++ qq67 ++ qName qn ++ '|' : bq k xs
        bOq qn k (x:xs)
          | isLetter x         = bOq (x:qn) k xs
          | otherwise          = b k (reverse qn++x:xs)

        -- brace quasi-quotation {qq|...|}
        bq _ ""                = error "unterminated haskell quasi-quotation using curly braces in french quotes (expecting `|}')"
        bq k ('|':'}':xs)      = '|' : ']' : k xs
        bq k (x:xs)            = x : bq k xs

        -- braces (haskell) context
        b _ ""                 = error "unterminated quotes hole using curly braces (expecting `}')"
        b k ('«':xs)           = openFrQQ ++ f ((closeFrQQ++) . b k) xs
        b _ ('»':_)            = error "unexpected closing french quote"
        b k ('{':'-':xs)       = "{-" ++ c (("-}"++) . b k) xs
        b k ('{':xs)           = '{' : b (('}':) . b k) xs
        b _ ('|':'}':_)        = error "unexpected `|}' (end of braced quasi-quotation) in a braced hole context"
        b k ('}':xs)           = k xs
        b k ('[':'$':xs)       = '[' : hOq "" (b k) xs
        b k ('[':xs)           = '[' : hOq "" (b k) xs
        b k ('"':xs)           = '"' : s (('"':) . b k) xs
        b _ ('-':'-':xs)       = mc (\_-> error "unexpected one line haskell comment (as in \"-- foo\") in curly braces") xs
        b k (x:'-':'-':xs)     | x == ':' || isAscSymb x = x : '-' : '-' : breakWith (=='-') k xs
        b k (x:xs)             = x : b k xs

        -- haskell (nested) comments
        c _ ""                 = error "unterminated haskell comment (expecting `-}')"
        c k ('{':'-':xs)       = "{-" ++ c (("-}"++) . c k) xs
        c k ('-':'}':xs)       = k xs
        c k (x:xs)             = x : c k xs

        -- haskell strings literal
        s _ ""                 = error "unterminated haskell string (expecting `\"')"
        s k ('\\':x:xs)        = '\\' : x : s k xs
        s k ('"':xs)           = k xs
        s k (x:xs)             = x : s k xs

        -- haskell char literal (a bit lenient)
        a _ ""                 = error "unterminated haskell character (expecting `'')"
        a k ('\\':x:xs)        = '\\' : x : a k xs
        a k (x:'\'':xs)
                   | x /= '\'' = x : '\'' : k xs
        a k xs                 = k xs

        -- haskell quasi-quotation
        -- is there nested QQ?
        q _ ""                 = error "unterminated haskell quasi-quotation (expecting `|]')"
        q k ('|':']':xs)       = '|' : ']' : k xs
        q k (x:xs)             = x : q k xs

        -- We've seen a `[' is it an Haskell list or a quasi-quotation
        hOq qn k ""            = k (reverse qn)
        hOq qn k ('|':xs)      = qq67 ++ qName qn ++ '|' : q k xs
        hOq qn k (x:xs)
          | isLetter x         = hOq (x:qn) k xs
          | otherwise          = k (reverse qn++x:xs)
