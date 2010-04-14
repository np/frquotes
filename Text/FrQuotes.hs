module Text.FrQuotes (frQuotes) where

frTop, openFrQQ', closeFrQQ', openFrQQ, closeFrQQ,
  openFrQ, closeFrQ, openBr, closeBr :: String
frTop = "(frTop ("
openFrQQ'  = "[$frQQ|"
closeFrQQ' = "|]"
openFrQQ  = frTop ++ openFrQQ'
closeFrQQ = closeFrQQ' ++ "))"
openFrQ   = "\xc2\xab"
closeFrQ  = "\xc2\xbb"
openBr    = closeFrQQ' ++ " `mappend` frAntiq ("
closeBr   = ") `mappend` " ++ openFrQQ'

-- Substitutes UTF8 french quotes «...» for (frTop [$frQQ|...|])
-- Antiquotations are supported via braces {...} and are
-- substituted for frAntiq, blocks are catenated with mappend.
-- Here String is used as UTF-8 code points.
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
        h ('\xc2':'\xab':'{':xs) = frTop ++ "( frAntiq (" ++ b ((closeBr++) . f ((closeFrQQ++) . (')':) . h)) xs -- avoid an empty [$frQQ||]
        h ('\xc2':'\xab':xs)   = openFrQQ ++ f ((closeFrQQ++) . h) xs
        h ('{':'-':xs)         = "{-" ++ c (("-}"++) . h) xs
        h ('"':xs)             = '"' : s (('"':) . h) xs
        h ('\'':xs)            = '\'' : a h xs
        h ('[':'$':xs)         = '[' : '$' : startq h xs
        h (x:xs)               = x : h xs

        -- french quotes context
        f _ ""                 = error "unterminated french quotes (expecting `\xc2\xbb')"
        f _ ('}':_)            = error "unexpected closing brace `}'"
        f k ('{':xs)           = openBr  ++ b ((closeBr++)  . f k) xs
        f k ('\xc2':'\xab':xs) = openFrQ ++ f ((closeFrQ++) . f k) xs
        f k ('\xc2':'\xbb':xs) = k xs
        f k (x:xs)             = x : f k xs

        -- braces (haskell) context
        b _ ""                 = error "unterminated quotes hole using curly braces (expecting `}')"
        b k ('\xc2':'\xab':xs) = openFrQQ ++ f ((closeFrQQ++) . b k) xs
        b _ ('\xc2':'\xbb':_)  = error "unexpected closing french quote"
        b k ('{':'-':xs)       = "{-" ++ c (("-}"++) . b k) xs
        b k ('{':xs)           = '{' : b (('}':) . b k) xs
        b k ('}':xs)           = k xs
        b k ('"':xs)           = '"' : s (('"':) . b k) xs
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
        startq k xs = ys ++ '|' : q k zs'
          where (ys,zs) = break (=='|') xs
                zs' | null zs   = error "unrecognized haskell quasi-quotation (expecting `|`)"
                    | otherwise = drop 1 zs
