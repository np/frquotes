import System.Environment
import System.IO
import Text.FrQuotes (frQuotes)

-- | The 'readBinaryFile' function reads a binary file and
-- returns the contents of the file as a string.
-- The file is read lazily, on demand, as with 'getContents'.

readBinaryFile        :: FilePath -> IO String
readBinaryFile name   =  openBinaryFile name ReadMode >>= hGetContents

-- | The computation 'writeBinaryFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f txt = withBinaryFile f WriteMode (\ hdl -> hPutStr hdl txt)

interact' :: (String -> String) -> IO ()
interact' f = do
  args <- getArgs
  case args of
    [] -> interact f
    [in1,inp,outp] ->
          writeBinaryFile outp . g . f =<< readBinaryFile inp
            where g = (("{-# LINE 1 \""++in1++"\" #-}\n")++)
    _  -> fail "Usage: frquotes [<orig> <input> <output>]"

main :: IO ()
main = interact' frQuotes
