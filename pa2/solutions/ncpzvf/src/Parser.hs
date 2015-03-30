module Parser
( parseRust
) where

import Syntax
import qualified Text.Parsec.Token as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import Text.Parsec.Char (char, alphaNum, letter, digit, oneOf)
import Text.Parsec.Prim (many, try)
import Text.Parsec.Combinator (eof,sepBy,sepBy1,option,many1,notFollowedBy
                              ,optionMaybe, choice, chainl1, optional)
import Control.Applicative ((<|>), (<*>), (<*), (*>), (<$>), (<$))

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
          "f32", "f64", "usize", "isize", "char", "str", "!", "Box"
        ]

literals = [ "true", "false" ]

operators = [ "::", "->", "=>", "#!","#", "'", "$", "[", "]"
            , "(", ")", "{", "}", ",", ";", ":", "."
            ]

parser = T.makeTokenParser rust

identifier    = T.identifier parser
reserved      = T.reserved parser
operator      = T.operator parser
reservedOp    = T.reservedOp parser
comma         = T.comma parser
semi          = T.semi parser
colon         = T.colon parser
whiteSpace    = T.whiteSpace parser
parens        = T.parens parser
braces        = T.braces parser
brackets      = T.brackets parser
angles        = T.angles parser
lexeme        = T.lexeme parser
charLiteral   = T.charLiteral parser
stringLiteral = T.stringLiteral parser
symbol        = T.symbol parser

parseRust :: String -> Either P.ParseError Crate
parseRust = P.parse (whiteSpace *> crate <* eof) ""

crate = Crate <$> many item

item =  fnDef
    <|> enumDef
    <|> structDef

fnDef = do
    reserved "fn"
    name   <- Id <$> identifier
    params <- parens (fnParam `sepBy` comma)
    typ    <- optionMaybe fnType
    stmts  <- braces (many stmt)
    return $ FnDef name params typ stmts

fnParam = do
    p <- pattern
    reservedOp ":"
    t <- dType
    return $ FnParam p t

fnType = do
    reservedOp "->"
    t <- dType
     <|> TypeNone <$ reserved "!"
    return t

enumDef = do
    reserved "enum"
    name  <- Id <$> identifier
    ctors <- braces (enumCtorDef `sepBy1` comma)
    return $ EnumDef name ctors

enumCtorDef = do
    name   <- Id <$> identifier
    params <- option [] $ parens (dType `sepBy` comma)
    return $ EnumCtorDef name params

structDef = do
    reserved "struct"
    name   <- Id <$> identifier
    fields <- braces (fieldDef `sepBy1` comma)
    return $ StructDef name fields

fieldDef = do
    name <- Id <$> identifier
    colon
    typ  <- dType
    return $ FieldDef name typ

stmt =  stmtLet
    <|> stmtReturn
    <|> expression
    <|> expressionSansSemi

stmtLet = do
    reserved "let"
    p <- pattern
    t <- optionMaybe (reservedOp ":" *> dType)
    e <- optionMaybe (reservedOp "=" *> expr)
    semi
    return $ Let p t e

stmtReturn = do
    reserved "return"
    val <- expr
    semi
    return $ Return val

expression = try $ do
    e <- expr
    semi
    return $ Expression e

expressionSansSemi = try $ do
    e <- expr
    notFollowedBy semi
    notFollowedBy stmt
    return $ Expression e

expr =  try binExpr
    <|> try fieldLookup
    <|> try arrIndex
    <|> simpleExpr

simpleExpr =  arr
          <|> boxNew
          <|> match
          <|> while
          <|> eIf
          <|> loop
          <|> literal
          <|> enumCtor
          <|> struct
          <|> fnCall
          <|> idExp
          <|> unit
          <|> parens expr

arr = Arr <$> brackets (expr `sepBy` comma)

loop = do
    reserved "loop"
    stmts <- braces (many stmt)
    return $ Loop stmts

literal = Literal <$> lit

lit =  LitChar  <$ (try $ char 'b' >> charLiteral)
   <|> LitStr   <$ (try $ char 'b' >> stringLiteral)
   <|> LitDec   <$ (optional (symbol "-") >> uDecimal)
   <|> LitTrue  <$ reserved "true"
   <|> LitFalse <$ reserved "false"

uDecimal = lexeme $ many1 uDigit

uDigit = digit <* (many $ char '_')

boxNew = do
    reserved "Box"
    reservedOp "::"
    reserved "new"
    arg <- parens expr
    return $ BoxNew [arg]

match = do
    reserved "match"
    e    <- expr
    arms <- braces (matchArm `sepBy` comma)
    return $ Match e arms

matchArm = do
    pats  <- pattern `sepBy` (symbol "|")
    reservedOp "=>"
    stmts <- braces (many stmt)
    return $ MatchArm pats stmts

while = do
    reserved "while"
    e  <- expr
    stmts <- braces (many stmt)
    return $ While e stmts

eIf = do
    reserved "if"
    e  <- expr
    ts <- braces (many stmt)
    fs <- option [] $ reserved "else" >> braces (many stmt)
    return $ If e ts fs

enumCtor = try $ do
    enum <- Id <$> identifier
    reservedOp "::"
    ctor <- Id <$> identifier
    es   <- option [] $ parens (expr `sepBy` comma)
    return $ EnumCtor enum ctor es

struct = try $ do
    n      <- Id <$> identifier
    fields <- braces (fieldInit `sepBy` comma)
    return $ Struct n fields

fieldInit = do
    n <- Id <$> identifier
    reservedOp ":"
    e <- expr
    return $ FieldInit n e

fnCall = try $ do
    n  <- Id <$> identifier
    es <- parens (expr `sepBy` comma)
    return $ FnCall n es

idExp = IdExp <$> Id <$> identifier

unit = Unit <$ try (symbol "()")

-- TODO: consider putting fieldLookup, arrIndex? into binExpr
fieldLookup = do
    e <- simpleExpr
    fieldLookupSuffix e

fieldLookupSuffix e = do
    reservedOp "."
    n <- Id <$> identifier
    tryExprSuffix (FieldLookup e n)

arrIndex = do
    e <- simpleExpr
    arrIndexSuffix e

arrIndexSuffix e = do
    e' <- brackets expr
    tryExprSuffix (ArrIndex e e')

tryExprSuffix e =  fieldLookupSuffix e
               <|> arrIndexSuffix e
               <|> return e

binExpr = E.buildExpressionParser binOpTable binTerm

binOpTable = unaryOpTable ++ numOpTable ++ boolOpTable ++ asgnOpTable

binTerm =  try fieldLookup
       <|> try arrIndex
       <|> parens binExpr
       <|> simpleExpr

unaryOpTable = [ [ prefix $  un "-" (NumExp . Neg)
                         <|> un "!" (BoolExp . Not)
                         <|> uns ["&", "mut"] (map (PointerExp .) [ AddrOf, AddrOfMut ])
                         <|> un "*" (PointerExp . Deref)
                 ]
               ]

numOpTable = [ [ bin "*" ((NumExp .) . Mul), bin "/" ((NumExp .) . Div)
               , bin "%" ((NumExp .) . Rem) ]
             , [ bin "+" ((NumExp .) . Add), bin "-" ((NumExp .) . Sub) ]
             ]

boolOpTable = [ [ bin ">"  ((BoolExp .) . Gt),  bin "<"  ((BoolExp .) . Lt)
                , bin ">=" ((BoolExp .) . GEq), bin "<=" ((BoolExp .) . LEq)
                , bin "==" ((BoolExp .) . Eq),  bin "!=" ((BoolExp .) . NEq) ]
              , [ bin "&&" ((BoolExp .) . And) ]
              , [ bin "||" ((BoolExp .) . Or) ]
              ]

asgnOpTable = [ [ bin "+=" (Assign AsgnAdd), bin "-=" (Assign AsgnSub)
                , bin "*=" (Assign AsgnMul), bin "/=" (Assign AsgnDiv)
                , bin "%=" (Assign AsgnRem), bin "="  (Assign Asgn) ]
              ]

-- optable helpers
uns ops ctors = uns' ops ctors
    where
        uns' []    _      = fail ""
        uns' _     []     = fail ""
        uns'(o:os) (c:cs) = do
            symbol o
            ctor <- option c $ uns' os cs
            return ctor

-- chainl1 is workaround for buildExpressionParser not allowing repeated unary ops
-- http://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
prefix p    = E.Prefix . chainl1 p $ return (.)
un  op ctor = (symbol op >> return ctor)
bin op ctor = E.Infix (reservedOp op >> return ctor) E.AssocLeft

pattern =  PatWild  <$  symbol "_"
       <|> PatLit   <$> lit
       <|> PatArr   <$> brackets (pattern `sepBy` comma)
       <|> PatDeref <$> (symbol "&" *> pattern)
       <|> patStruct
       <|> patEnum
       <|> patRef
       <|> PatMutId <$> Id <$> (reserved "mut" *> identifier)
       <|> PatId    <$> Id <$> identifier
       <|> PatUnit  <$  symbol "()"

patStruct = try $ do
    n      <- Id <$> identifier
    fields <- braces (fieldPat `sepBy` comma)
    return $ PatStruct n fields

fieldPat = do
    n <- Id <$> identifier
    reservedOp ":"
    p <- pattern
    return $ FieldPat n p

patEnum = try $ do
    enum <- Id <$> identifier
    reservedOp "::"
    ctor <- Id <$> identifier
    pats <- option [] $ parens (pattern `sepBy` comma)
    return $ PatEnum enum ctor pats

patRef = do
    reserved "ref"
    ctor <- option PatRefId $ (PatRefMutId <$ reserved "mut")
    n    <- Id <$> identifier
    return $ ctor n

dType =  typeRef
     <|> TypeI32  <$  reserved "i32"
     <|> TypeBool <$  reserved "bool"
     <|> TypeU8   <$  reserved "u8"
     <|> TypeArr  <$> brackets arrDef
     <|> TypeBox  <$> (reserved "Box" *> angles dType)
     <|> TypeId   <$> Id <$> identifier
     <|> TypeUnit <$  symbol "()"

typeRef = do
    reservedOp "&"
    ctor <- option TypeRef (TypeRefMut <$ reserved "mut")
    t    <- dType
    return $ ctor t

arrDef = do
    t <- dType
    e <- optionMaybe (reservedOp ";" *> expr)
    return $ ArrDef t e
