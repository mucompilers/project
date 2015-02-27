module Rust.Lexer (lexRust, Token(..)) where

import           Data.Functor.Identity
import           Text.Parsec
import           Text.Parsec.Language  (LanguageDef, emptyDef)
import qualified Text.Parsec.Token     as PT

-- | The possible forms of tokens which can be lexed.
data Token = Keyword String
           | Identifier String
           | LitChar Char
           | LitString String
           | LitDecimal Integer
           | LitBool Bool
           | Operator String
           | Symbol String
           | PrimitiveType String
           deriving (Show, Eq)

-- | Lexes a program defined in the Rust format.
-- | Takes a file name and its contents as parameters in order
-- | to provide useful error messages and lex.
lexRust :: SourceName -> String -> Either ParseError [Token]
lexRust =
  let parsers = [ keyword, primitiveType, symbol, operator
                , charStringLiteral, decimalLiteral, boolLiteral
                , identifier ]
  in runParser (manyTill (choice parsers) eof) ()

-- | The language format for Rust.
rustStyle :: LanguageDef st
rustStyle = emptyDef
            { PT.commentStart    = "/*",
              PT.commentEnd      = "*/",
              PT.commentLine     = "//",
              PT.nestedComments  = True,
              PT.identStart      = letter <|> char '_',
              PT.identLetter     = alphaNum <|> char '_',
              PT.opStart         = PT.opLetter rustStyle,
              PT.opLetter        = oneOf "+-*/%&|^<>=!",
              PT.reservedNames   = reservedNames
                                   ++ reservedPrimitiveTypes
                                   ++ ["true", "false"],
              PT.reservedOpNames = reservedOpNames ++ reservedSymbols,
              PT.caseSensitive   = True }

-- | The set of reserved names (keywords).
reservedNames :: [String]
reservedNames = [ "abstract", "alignof", "as", "be", "box"
                , "break", "const", "continue", "crate"
                , "do", "else", "enum", "extern"
                , "final", "fn", "for", "if", "impl", "in"
                , "let", "loop", "macro", "macro_rules"
                , "match", "mod", "move", "mut", "offsetof"
                , "override", "priv", "pub", "pure", "ref"
                , "return", "sizeof", "static", "self"
                , "struct", "super", "trait", "type"
                , "typeof", "unsafe", "unsized", "use"
                , "virtual", "where", "while", "yield" ]

-- | The set of reserved primitive types.
reservedPrimitiveTypes :: [String]
reservedPrimitiveTypes = [ "()", "bool", "u8", "u16", "u32", "u64"
                         , "i8", "i16", "i32", "i64", "f32", "f64"
                         , "usize", "isize", "char", "str" ]

-- | The set of reserved op names.
reservedOpNames :: [String]
reservedOpNames = [ "+", "-", "*", "/", "%", "&", "|", "^", "!"
                  , "<<", ">>", "||", "&&", "==", "!=", "<"
                  , ">", "<=", ">=", "+=", "-=", "*=", "/=", "%="
                  , "&=", "|=", "^=", "<<=", ">>=" ]

-- | The set of reserved symbols.
reservedSymbols :: [String]
reservedSymbols = [ "::", "->", "=>", "#", "#!", "'", "$", "="
                  , "[", "]", "(", ")", "{", "}", ",", ";" ]

-- | The general token parser for the Rust language.
tokenParser :: PT.GenTokenParser String u Identity
tokenParser = PT.makeTokenParser rustStyle

-- | The token parser for identifiers.
identifier :: ParsecT String u Identity Token
identifier = do
  s <- try $ PT.identifier tokenParser
  return $ Identifier s

-- | Creates a keyword parser for a specific keyword.
makeReserved :: (String -> Token)
             -> (String -> ParsecT String u Identity ())
             -> String
             -> ParsecT String u Identity Token
makeReserved c f s = do
  try $ f s
  return $ c s

-- | The parser for all types of keywords.
-- | Will return the token.
keyword :: ParsecT String u Identity Token
keyword =
  let names    = reservedNames
      make     = makeReserved Keyword $ PT.reserved tokenParser
      keywords = map make names
  in choice keywords

-- | The parser for character and string literals.
charStringLiteral :: ParsecT String u Identity Token
charStringLiteral =
  try (char 'b') >> (string' <|> char')
  where string' = do
          s <- PT.stringLiteral tokenParser
          notFollowedBy (letter <|> char '_')
          return $ LitString s
        char' = do
          c <- PT.charLiteral tokenParser
          notFollowedBy (letter <|> char '_')
          return $ LitChar c

-- | The parser for decimal literals.
decimalLiteral :: ParsecT String u Identity Token
decimalLiteral =
  let under    = many $ try (char '_')
      decimal' = sepEndBy1 digit under
  in do
    ds <- try decimal'
    notFollowedBy (letter <|> char '_')
    _ <- many space
    return $ LitDecimal (read ds :: Integer)

-- | The parser for boolean literals.
boolLiteral :: ParsecT String u Identity Token
boolLiteral =
  let true = do
        PT.reserved tokenParser "true"
        return $ LitBool True
      false = do
        PT.reserved tokenParser "false"
        return $ LitBool False
  in true <|> false

-- | The parser for operators.
operator :: ParsecT String u Identity Token
operator =
  let ops  = reservedOpNames
      make = makeReserved Operator $ PT.reservedOp tokenParser
      ops' = map make ops
  in choice ops'

-- | The parser for symbols.
symbol :: ParsecT String u Identity Token
symbol =
  let syms  = reservedSymbols
      make  = makeReserved Symbol $ PT.reservedOp tokenParser
      syms' = map make syms
  in choice syms'

-- | The parser for primitive types.
primitiveType :: ParsecT String u Identity Token
primitiveType =
  let ts   = reservedPrimitiveTypes
      make = makeReserved PrimitiveType $ PT.reserved tokenParser
      ts'  = map make ts
  in choice ts'
