module Syntax where

data Token = ID String
           | Keyword String
           | Literal Lit
           | Symbol String
           | Operator String
           | Type String

data Lit = LitChar Char
         | LitStr String
         | LitDec Integer
         | LitBool Bool

instance Show Token where
    show (ID s)       = "ID(" ++ s ++ ")"
    show (Keyword s)  = s
    show (Literal l)  = show l
    show (Symbol s)   = s
    show (Operator s) = s
    show (Type s)     = s

instance Show Lit where
    show (LitChar c)     = "LITCHAR(" ++ [c] ++ ")"
    show (LitStr s)      = "LITSTR(" ++ s ++ ")"
    show (LitDec n)      = "LITDEC(" ++ show n ++ ")"
    show (LitBool True)  = "LITBOOL(true)"
    show (LitBool False) = "LITBOOL(false)"
