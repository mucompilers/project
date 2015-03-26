module Lexer where

import Data.Char

-- The set of possiblie symbols in the 'Expression Language'
data Sym = Plus |
           Minus |
           Multiply |
           Divide
         deriving Eq

instance Show Sym where
  show Plus     = "+"
  show Minus    = "-"
  show Multiply = "*"
  show Divide   = "/"

-- The set of possible tokens in the 'Expression Language'
data Token = LParen |
             RParen |
             Number Int |
             Symbol Sym
           deriving Eq

instance Show Token where
  show LParen     = "("
  show RParen     = ")"
  show (Number x) = show x
  show (Symbol x) = show x

-- Lexes (tokenizes) an 'Expression Language' string
-- Into a set of matchable symbols.
lexExp :: String -> [Token]
lexExp s =
  let s' = filter (/= ' ') s
      match '(' ts = LParen : ts
      match ')' ts = RParen : ts
      match '+' ts = Symbol Plus : ts
      match '-' ts = Symbol Minus : ts
      match '*' ts = Symbol Multiply : ts
      match '\\' ts = Symbol Divide : ts
      match c (t : ts)
        | isDigit c =
            case t of
             Number x -> Number (x * 10 + digitToInt c) : ts
             _        -> Number (digitToInt c) : t : ts
        | otherwise = error ("Invalid character: " ++ [c])
      match c []
        | isDigit c = [Number (digitToInt c)]
        | otherwise = error ("Invalid character: " ++ [c])
  in foldr match [] s'
