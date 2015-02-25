import Lexer
import Data.List (intercalate)
import System.IO (isEOF)
import Control.Applicative ((<$>))

main = loop

{- Parsing one line at a time using getLine,
   this will cause comment lines to show up as blank lines in output -}
loop = do end <- isEOF
          if end
              then putStr ""
              else do line <- prettyPrint <$> lexRust <$> getLine
                      putStrLn line
                      loop

prettyPrint (Left e)   = show e
prettyPrint (Right ts) = intercalate "\n" $ map show ts
