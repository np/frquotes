import System.Environment
import System.IO
import Text.FrQuotes (frQuotes)

readFileEnc :: TextEncoding -> FilePath -> IO String
readFileEnc enc fp = do
  fh <- openFile fp ReadMode
  hSetEncoding fh enc
  hGetContents fh

writeFileEnc :: TextEncoding -> FilePath -> String -> IO ()
writeFileEnc enc fp s =
  withFile fp WriteMode $ \fh ->
    hSetEncoding fh enc >>
    hPutStr fh s

interact' :: TextEncoding -> (String -> String) -> IO ()
interact' enc f = do
  args <- getArgs
  case args of
    [] -> do
      hSetEncoding stdin  enc
      hSetEncoding stdout enc
      interact f
    [in1,inp,outp] ->
      writeFileEnc enc outp . g . f =<< readFileEnc enc inp
      where g = (("{-# LINE 1 \""++in1++"\" #-}\n")++)
    _  -> fail "Usage: frquotes [<orig> <input> <output>]"

main :: IO ()
main = interact' utf8 frQuotes
