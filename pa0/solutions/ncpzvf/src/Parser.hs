module Parser
( parse
) where

import Data.Char (isDigit, isSpace)
import Syntax

parse :: String -> Exp
parse s = parse' s []

parse' :: String -> [Exp] -> Exp
parse' [] [e] = e
parse' [] _   = error "Malformed expression"
parse' (s:ss) stack
    | isSpace s = parse' ss stack
    | isDigit s = parse' rem (num : stack)
    where
        (num, rem) = litNum (s:ss)

parse' ('(':ss) stack =
    let (exp, rem) = matchParen ss
        expStack   = parse' exp []
    in  parse' rem (expStack : stack)

parse' ('+':ss) stack  = parseOp ss stack Add
parse' ('-':ss) stack  = parseOp ss stack Sub
parse' ('*':ss) stack  = parseOp ss stack Mul
parse' ('\\':ss) stack = parseOp ss stack Div

-- Catch all
parse' _ _ = error "Malformed expression"

parseOp s [e] exp = exp e (parse' s [])
parseOp _ _ _     = error "Malformed expression"

matchParen s = matchParen' s 0 ""
    where
        matchParen' (')':ss) 0 stack = (reverse stack,ss)
        matchParen' (')':ss) n stack = matchParen' ss (n-1) (')':stack)
        matchParen' ('(':ss) n stack = matchParen' ss (n+1) ('(':stack)
        matchParen' (s  :ss) n stack = matchParen' ss n (s:stack)

litNum :: String -> (Exp, String)
litNum (s:ss)
    | isSpace s = litNum ss
    | otherwise = (LitNum (read num), rem)
    where
        (num, rem) = break (not . isDigit) (s:ss)
