--import System.Environment (getArgs)

import Control.Monad
import System.IO
import Language
import Lexer
import Parser

-- The entry-point of the application.
main :: IO ()
main = do
  -- Output the prompt...
  putStr " # "
  hFlush stdout

  -- And get and process the input.
  s <- getLine
  unless (s == "exit") $ do
    print . toSML . parse . lexExp $ s
    main
