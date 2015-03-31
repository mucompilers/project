#Terry Ballou-Crawford
#14163546
#3/16/15
#Programming Assignment 2

import lex as lex
import sys

data = None
if len(sys.argv) == 2:
    text_file = open(sys.argv[1], "r")
    data = text_file.read()
    text_file.close

tokens = (
          "BOOL", "ID", "KEYWORD", "TYPE",
          "OPERATOR", "NUMBER", "SYMBOL", "CHAR",
          "STRING", "NEWLINE", "COMMENT", "LET",
          "FUNCTION", "COLON", "SEMICOLON", "EQ",
          "LPAREN", "RPAREN", "LBRACK", "RBRACK",
          "MINUS", "COMMA", "NOT", "RARROW", "STRUCT",
          "LOOP", "LEQ", "GEQ", "BOX", "DOT", "OR", "RETURN",
          "DOUBLECOLON", "NEW", "ENUM", "NIL", "LSQBRACK",
          "RSQBRACK", "DEREF", "REF", "MUT", "WHILE",
          "IF", "ELSE", "MATCH", "SINGLEOR"
          )

def t_WHILE(t):
    r'while'
    return t

def t_IF(t):
    r'if'
    return t

def t_ELSE(t):
    r'else'
    return t

def t_MATCH(t):
    r'match'
    return t

def t_REF(t):
    r'ref'
    return t

def t_MUT(t):
    r'mut'
    return t

def t_LSQBRACK(t):
    r'[\[]'
    return t

def t_RSQBRACK(t):
    r'[\]]'
    return t

def t_NIL(t):
    r'Nil'
    return t

def t_ENUM(t):
    r'enum'
    return t

def t_DOUBLECOLON(t):
    r'[:][:]'
    return t

def t_BOOL(t):
    r"true|false"
    return t
 
def t_CHAR(t):
    r'''[b][\'][\']*[\']|
    [b][\'][\\]*[\']|
    [b][\'].[\']'''
    return t
 
def t_LOOP(t):
    r'loop'
    return t
 
def t_STRING(t):
    r'b\"([\^\"]|"\\\"")+\"'
    return t
     
# Check that these work or not
def t_COMMENT(t):
    r'''[//][//][//].*|
    [//][\*].*.[\*][//]|
    [//][//].*'''
    pass
 
def t_SYMBOL(t):
    r"""
    [:][:]|[=][>]|[#][!]|[#]|[']|[$]
    """
    return t
 
def t_OR(t):
    r'[|][|]'
    return t
 
def t_OPERATOR(t):
    r"""
    [<][<][=]|[>][>][=]|[.][.][.]|[.][.]|[<][<]|[>][>]|
    [&][&]|[!][=]|[<][=]|[>][=]|[+][=]|[-][=]|[*][=]|[/][=]|
    [%][=]|[&][=]|[|][=]|[\^][=]|[#][!]|[=][=]|
    [\*]|[+]|[/]|[%]|[\^]
    """
    return t

def t_SINGLEOR(t):
    r'[|]'
    return t

def t_DEREF(t):
    r'[&]'
    return t

def t_DOT(t):
    r'[.]'
    return t
    
def t_STRUCT(t):
    r'struct'
    return t
    
def t_TYPE(t):
    r"""
    bool|u8|u16|u32|u64|i8|i16|i32|
    i64|f32|f64|usize|isize|char|str
    """
    return t
    
def t_BOX(t):
    r'Box'
    return t
    
def t_FUNCTION(t):
    r'fn'
    return t
    
def t_LET(t):
    r'let'
    return t
    
def t_COLON(t):
    r':'
    return t
    
def t_SEMICOLON(t):
    r';'
    return t

def t_EQ(t):
    r'='
    return t

def t_LPAREN(t):
    r'[(]'
    return t

def t_RPAREN(t):
    r'[\)]'
    return t

def t_LBRACK(t):
    r'{'
    return t

def t_RBRACK(t):
    r'}'
    return t
    
def t_KEYWORD(t):
    r"""
    ^abstract$|^alignof$|^as$|^be$|^break$|
    ^const$|^continue$|^crate$|^do$|
    ^extern$|^final$|^for$|^impl$|^in$|
    ^macro_rules$|^macro$|
    ^mod$|^move$|^mut$|^offsetof$|^override$|^priv$|
    ^pub$|^pure$|^ref$|^sizeof$|^static$|
    ^self$|^super$|^trait$|^type$|^typeof$|
    ^unsafe$|^unsized$|^use$|^virtual$|^where$|^while$|
    ^yield$
    """
    return t

def t_NEW(t):
    r'new'
    return t

def t_RETURN(t):
    r'return'
    return t

def t_ID(t):
    r"""
    [a-zA-Z_]([_a-zA-Z0-9]+)?
    """
    return t
 
def t_NUMBER(t):
    r"""
    [0-9]([0-9_]+)?
    """
    return t

def t_RARROW(t):
    r'->'
    return t

def t_MINUS(t):
    r'-'
    return t

def t_COMMA(t):
    r','
    return t

def t_NOT(t):
    r'!'
    return t
 
def t_LEQ(t):
    r'<'
    return t

def t_GEQ(t):
    r'>'
    return t
 
def t_NEWLINE(t):
    r"\n"
    t.lexer.lineno += 1
    pass

t_ignore = '\t '
    
def t_error(t):
    print("Illegal character '" + t.value[0] 
          + "' at line " + str(t.lexer.lineno))
    t.lexer.skip(1)
   

   
data = r""""""

lexer = lex.lex(debug=0)
# lexer.input('')
#          
# while True:
#     tok = lexer.token()
#     if not tok: break
# #     if tok.type == 'ID' or tok.type == 'DECIMAL':
# #         print(tok.type + "(" + tok.value + ")")
# #     else:
#     print(tok.type + "(" + tok.value + ")")