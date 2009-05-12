import System.Environment

-- substitutes UTF8 french quotes «...» for [$e|...|]

interact' :: (String -> String) -> IO ()
interact' f = do
  args <- getArgs
  case args of
    [] -> interact f
    [in1,inp,outp] -> writeFile outp =<< ((("{-# LINE 2 \""++in1++"\" #-}")++) . f)
                                `fmap` readFile inp
    _  -> fail "Usage: frquotes [orig input output]"

openFrQQ'  = "[$frQQ|"
closeFrQQ' = "|]"
openFrQQ  = '(' : openFrQQ'
closeFrQQ = closeFrQQ' ++ ")"
openFrQ   = "\xc2\xab"
closeFrQ  = "\xc2\xbb"
openBr    = closeFrQQ' ++ " `mappend` ("
closeBr   = ") `mappend` " ++ openFrQQ'

main :: IO ()
main = interact' h
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
        h ('\xc2':'\xab':'{':xs) = "(" ++ b (((')':openFrQQ)++) . f ((closeFrQQ++) . h)) xs -- avoid an empty [$frQQ||]
        h ('\xc2':'\xab':xs)   = openFrQQ ++ f ((closeFrQQ++) . h) xs
        h ('{':'-':xs)         = "{-" ++ c (("-}"++) . h) xs
        h ('"':xs)             = '"' : s (('"':) . h) xs
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
        b _ ('\xc2':'\xbb':xs) = error "unexpected closing french quote"
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
