module Lexer
( lexRust
) where

import Syntax
import qualified Text.Parsec.Token as T
import qualified Text.Parsec as P
import Text.Parsec.Char (letter, alphaNum, char, oneOf, digit)
import Text.Parsec.Prim (try, many)
import Text.Parsec.Combinator (many1, eof)
import Control.Applicative ((<|>), (<$), (<$>), (<*), (*>))
import Data.List (foldl')

rust :: T.LanguageDef st
rust = T.LanguageDef
        { T.commentStart    = "/*"
        , T.commentEnd      = "*/"
        , T.commentLine     = "//"
        , T.nestedComments  = False
        , T.identStart      = letter <|> char '_'
        , T.identLetter     = alphaNum <|> char '_'
        , T.opStart         = oneOf "-*!+/%&|^<>=."
        , T.opLetter        = oneOf "&|=.<>"
        , T.reservedNames   = keywords ++ types ++ literals
        , T.reservedOpNames = operators
        , T.caseSensitive   = True
        }

keywords = [ "abstract", "alignof", "as", "be", "box", "break"
           , "const", "continue", "crate", "do", "else", "enum"
           , "extern", "final", "fn", "for", "if", "impl"
           , "in", "let", "loop", "macro", "macro_rules", "match"
           , "mod", "move", "mut", "offsetof", "override", "priv"
           , "pub", "pure", "ref", "return", "sizeof", "static"
           , "self", "struct", "super", "trait", "type"
           , "typeof", "unsafe", "unsized", "use", "virtual"
           , "where", "while", "yield"
           ]

types = [ "bool", "u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64",
          "f32", "f64", "usize", "isize", "char", "str"
        ]

literals = [ "true", "false" ]

operators = [ "::", "->", "=>", "#!","#", "'", "$", "[", "]"
            , "(", ")", "{", "}", ",", ";"
            ]

{- Use this to lex with _okay_ error messages -}
lexRust :: String -> Either P.ParseError [Token]
lexRust = P.parse (whiteSpace *> many token <* eof) ""

parser = T.makeTokenParser rust

identifier    = T.identifier parser
reserved      = T.reserved parser
operator      = T.operator parser
reservedOp    = T.reservedOp parser
comma         = T.comma parser
semi          = T.semi parser
whiteSpace    = T.whiteSpace parser
charLiteral   = T.charLiteral parser
stringLiteral = T.stringLiteral parser
lexeme        = T.lexeme parser

token =  Literal  <$> literal -- order matters here (byte literals)
     <|> ID       <$> identifier
     <|> Keyword  <$> keyword
     <|> Type     <$> primitive
     <|> Symbol   <$> symbol
     <|> Operator <$> operator

keyword = foldl' (<|>) P.parserZero $ map (\k -> k <$ reserved k) keywords

literal =  LitChar       <$> (try $ char 'b' >> charLiteral)
       <|> LitStr        <$> (try $ char 'b' >> stringLiteral)
       <|> LitDec        <$> uDecimal
       <|> LitBool True  <$ reserved "true"
       <|> LitBool False <$ reserved "false"

uDecimal = read <$> lexeme (many1 uDigit)

uDigit = digit <* (many $ char '_')

symbol = foldl' (<|>) P.parserZero $ map (\s -> s <$ reservedOp s) operators

primitive = foldl' (<|>) P.parserZero $ map (\t -> t <$ reserved t) types
