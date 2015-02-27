{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad      (liftM)
import           Data.Char
import           Rust.Lexer
import           System.Environment
import           Text.Parsec

main :: IO ()
main = do
  argc <- liftM length getArgs
  if argc < 1 then do
    text <- getContents
    printTokens $ lexRust "(stdin)" text
    --putStrLn "Error: Execution equires at least one file."
    --putStrLn "Proper usage: hw1 file.rs"
  else do
    file <- liftM (!! 0) getArgs
    text <- readFile file
    printTokens $ lexRust file text

printTokens :: Either ParseError [Token] -> IO ()
printTokens (Left e)   = print e
printTokens (Right ts) =
  let lower' x                 = map toLower $ show x
      print' (Keyword s)       = s ++ "\n"
      print' (Identifier s)    = "ID(" ++ s ++ ")\n"
      print' (LitChar c)       = "LITCHAR(" ++ [c] ++ ")\n"
      print' (LitString s)     = "LITSTR(" ++ s ++ ")\n"
      print' (LitDecimal d)    = "LITDEC(" ++ show d ++ ")\n"
      print' (LitBool b)       = "LITBOOL(" ++ lower' b ++ ")\n"
      print' (Operator s)      = s ++ "\n"
      print' (Symbol s)        = s ++ "\n"
      print' (PrimitiveType s) = s ++ "\n"
  in putStrLn $ foldl (\acc t -> acc ++ print' t) [] ts
