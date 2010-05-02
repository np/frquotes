import System.Environment
import Text.FrQuotes (frQuotes)

interact' :: (String -> String) -> IO ()
interact' f = do
  args <- getArgs
  case args of
    [] -> interact f
    [in1,inp,outp] ->
      writeFile outp . g . f =<< readFile inp
      where g = (("{-# LINE 1 \""++in1++"\" #-}\n")++)
    _  -> fail "Usage: frquotes [<orig> <input> <output>]"

main :: IO ()
main = interact' frQuotes
