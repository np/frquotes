import System.Environment
import Text.FrQuotes (frQuotes)

interact' :: (String -> String) -> IO ()
interact' f = do
  args <- getArgs
  case args of
    [] -> interact f
    [in1,inp,outp] -> writeFile outp =<< ((("{-# LINE 2 \""++in1++"\" #-}\n")++) . f)
                                `fmap` readFile inp
    _  -> fail "Usage: frquotes [<orig> <input> <output>]"

main :: IO ()
main = interact' frQuotes
