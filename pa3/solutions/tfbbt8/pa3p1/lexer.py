# Terry Ballou-Crawford
# 14163546 TFBBT8
# 2/18/2015
# PA1: Python Lexer using PLY
# ****ONLY TESTED USING PYTHON 3.4****

import lex as lex
import sys

data = None

tokens = ("ABSTRACT", "ALIGNOF", "AS", "BE", "BOX", "BREAK", "CONST", "CONTINUE",
          "CRATE", "DO", "ELSE", "ENUM", "EXTERN", "FINAL", "FN", "FOR", "IF",
          "IMPL", "IN", "LET", "LOOP", "MACRO", "MACRO_RULES", "MATCH", "MOD",
          "MOVE", "MUT", "OFFSETOF", "OVERRIDE", "PRIV", "PUB", "PURE", "REF",
          "RETURN", "SIZEOF", "STATIC", "SELF", "STRUCT", "SUPER", "TRAIT",
          "TYPE", "TYPEOF", "UNSAFE", "UNSIZED", "USE", "VIRTUAL", "WHERE",
          "WHILE", "YIELD",
          
          "NEW",
          
          "BOOL", "UEIGHT", "USIXTEEN", "UTHREETWO", "USIXFOUR", "IEIGHT",
          "ISIXTEEN", "ITHREETWO", "ISIXFOUR", "FTHREETWO", "FSIXFOUR", 
          "USIZE", "ISIZE", "CHAR", "STR",
          
          "CLASSACCESS", "OBJECTACCESS", "PIPE", "POUND", "POUNDEXCLAMATION",
          "APOSTRAPHE", "DOLLAR", "LBRACKET", "RBRACKET", "LPAREN", "RPAREN",
          "LSQUIGBRACKET", "RSQUIGBRACKET", "COMMA", "SEMICOLON", "COLON",
          
          "SUB", "MUL", "NOT", "ADD", "DIV", "MODOP", "AND", "OR", 
          "EXCLUSIVEOR", "LEFTSHIFT", "RIGHTSHIFT", "PERIOD", "WILDCARD",
          "THREEDOTS", "LOGICALOR", "LOGICALAND", "EQUALTO", "NOTEQUAL",
          "LESSTHAN", "GREATERTHAN", "LESSTHANOREQUAL", "GREATERTHANOREQUAL", 
          "PLUSEQUAL", "SUBEQUAL", "TIMESEQUAL", "DIVEQUAL", "MODEQUAL", 
          "ANDEQUAL", "OREQUAL", "EXCLUSIVEOREQUAL", "LEFTSHIFTEQUAL", 
          "RIGHTSHIFTEQUAL", "EQU", "WC",
          
          "LITCHAR", "LITSTRING", "LITDEC", "LITBOOL",
          
          "ID",
          
          "ERROR", "IGNORE", "NEWLINE")

def t_IGNORE(t):
    r'''[//][//][//].*|
    [//][\*].*.[\*][//]|
    [//][//].*'''
    pass

def t_NEWLINE(t):
    r"\n"
    t.lexer.lineno += 1
    pass

#KEYWORDS
def t_ABSTRACT(t):
    '\\babstract\\b'
    return t

def t_ALIGNOF(t):
    '\\balignof\\b'
    return t

def t_AS(t):
    '\\bas\\b'
    return t

def t_BE(t):
    '\\bbe\\b'
    return t

def t_BOX(t):
    '\\bbox\\b'
    return t

def t_BREAK(t):
    '\\bbreak\\b'
    return t

def t_CONST(t):
    '\\bconst\\b'
    return t

def t_CONTINUE(t):
    '\\bcontinue\\b'
    return t

def t_CRATE(t):
    '\\bcrate\\b'
    return t

def t_DO(t):
    '\\bdo\\b'
    return t

def t_ELSE(t):
    '\\belse\\b'
    return t

def t_ENUM(t):
    '\\benum\\b'
    return t

def t_EXTERN(t):
    '\\bextern\\b'
    return t

def t_FINAL(t):
    '\\bfinal\\b'
    return t

def t_FN(t):
    '\\bfn\\b'
    return t

def t_FOR(t):
    '\\bfor\\b'
    return t

def t_IF(t):
    '\\bif\\b'
    return t

def t_IMPL(t):
    '\\bimpl\\b'
    return t

def t_IN(t):
    '\\bin\\b'
    return t

def t_LET(t):
    '\\blet\\b'
    return t

def t_LOOP(t):
    '\\bloop\\b'
    return t

def t_MACRO(t):
    '\\bmacro\\b'
    return t

def t_MACRO_RULES(t):
    '\\bmacro_rules\\b'
    return t

def t_MATCH(t):
    '\\bmatch\\b'
    return t

def t_MOD(t):
    '\\bmod\\b'
    return t

def t_MOVE(t):
    '\\bmove\\b'
    return t

def t_MUT(t):
    '\\bmut\\b'
    return t

def t_OFFSETOF(t):
    '\\boffsetof\\b'
    return t

def t_OVERRIDE(t):
    '\\boverride\\b'
    return t

def t_PRIV(t):
    '\\bpriv\\b'
    return t

def t_PUB(t):
    '\\bpub\\b'
    return t

def t_PURE(t):
    '\\bpure\\b'
    return t

def t_REF(t):
    '\\bref\\b'
    return t

def t_RETURN(t):
    '\\breturn\\b'
    return t

def t_SIZEOF(t):
    '\\bsizeof\\b'
    return t

def t_STATIC(t):
    '\\bstatic\\b'
    return t

def t_SELF(t):
    '\\bself\\b'
    return t

def t_STRUCT(t):
    '\\bstruct\\b'
    return t

def t_SUPER(t):
    '\\bsuper\\b'
    return t

def t_TRAIT(t):
    '\\btrait\\b'
    return t

def t_TYPE(t):
    '\\btype\\b'
    return t

def t_TYPEOF(t):
    '\\btypeof\\b'
    return t

def t_UNSAFE(t):
    '\\bunsafe\\b'
    return t

def t_UNSIZED(t):
    '\\bunsized\\b'
    return t

def t_USE(t):
    '\\buse\\b'
    return t

def t_VIRTUAL(t):
    '\\bvirtual\\b'
    return t

def t_WHERE(t):
    '\\bwhere\\b'
    return t

def t_WHILE(t):
    '\\bwhile\\b'
    return t

def t_YIELD(t):
    '\\byield\\b'
    return t

#NEW
def t_NEW(t):
    '\\bnew\\b'
    return t

#TYPES
def t_BOOL(t):
    '\\bbool\\b'
    return t

def t_UEIGHT(t):
    '\\bu8\\b'
    return t

def t_USIXTEEN(t):
    '\\bu16\\b'
    return t

def t_UTHREETWO(t):
    '\\bu32\\b'
    return t

def t_USIXFOUR(t):
    '\\bu64\\b'
    return t

def t_IEIGHT(t):
    '\\bi8\\b'
    return t

def t_ISIXTEEN(t):
    '\\bi16\\b'
    return t

def t_ITHREETWO(t):
    '\\bi32\\b'
    return t

def t_ISIXFOUR(t):
    '\\bi64\\b'
    return t

def t_FTHREETWO(t):
    '\\bf32\\b'
    return t

def t_FSIXFOUR(t):
    '\\bf64\\b'
    return t

def t_USIZE(t):
    '\\busize\\b'
    return t

def t_ISIZE(t):
    '\\bisize\\b'
    return t

def t_CHAR(t):
    '\\bchar\\b'
    return t

def t_STR(t):
    '\\bstr\\b'
    return t

#SYMBOLS
def t_CLASSACCESS(t):
    '[:][:]'
    return t

def t_OBJECTACCESS(t):
    '[-][>]'
    return t

def t_PIPE(t):
    '[=][>]'
    return t

def t_POUNDEXCLAMATION(t):
    '[#][!]'
    return t

def t_POUND(t):
    '[#]'
    return t

def t_APOSTRAPHE(t):
    '[\']'
    return t

def t_DOLLAR(t):
    '[$]'
    return t

def t_LBRACKET(t):
    '[[]'
    return t

def t_RBRACKET(t):
    '[]]'
    return t

def t_LPAREN(t):
    '[(]'
    return t

def t_RPAREN(t):
    '[)]'
    return t

def t_LSQUIGBRACKET(t):
    '[{]'
    return t

def t_RSQUIGBRACKET(t):
    '[}]'
    return t

def t_COMMA(t):
    '[,]'
    return t

def t_SEMICOLON(t):
    '[;]'
    return t

def t_COLON(t):
    '[:]'
    return t

#OPERATORS
def t_PLUSEQUAL(t):
    '[+][=]'
    return t

def t_SUBEQUAL(t):
    '[-][=]'
    return t

def t_TIMESEQUAL(t):
    '[*][=]'
    return t

def t_DIVEQUAL(t):
    '[/][=]'
    return t

def t_MODEQUAL(t):
    '[%][=]'
    return t

def t_ANDEQUAL(t):
    '[&][=]'
    return t

def t_OREQUAL(t):
    '[|][=]'
    return t

def t_EXCLUSIVEOREQUAL(t):
    '[\^][=]'
    return t

def t_LEFTSHIFTEQUAL(t):
    '[<][<][=]'
    return t

def t_RIGHTSHIFTEQUAL(t):
    '[>][>][=]'
    return t

def t_LESSTHANOREQUAL(t):
    '[<][=]'
    return t

def t_GREATERTHANOREQUAL(t):
    '[>][=]'
    return t

def t_SUB(t):
    '[-]'
    return t

def t_MUL(t):
    '[*]'
    return t

def t_ADD(t):
    '[+]'
    return t

def t_DIV(t):
    '[/]'
    return t

def t_MODOP(t):
    '[%]'
    return t

def t_LOGICALAND(t):
    '[&][&]'
    return t

def t_AND(t):
    '[&]'
    return t

def t_EXCLUSIVEOR(t):
    '[\^]'
    return t

def t_LOGICALOR(t):
    '[|][|]'
    return t

def t_OR(t):
    '[|]'
    return t

def t_LEFTSHIFT(t):
    '[<][<]'
    return t

def t_RIGHTSHIFT(t):
    '[>][>]'
    return t

def t_THREEDOTS(t):
    '[.][.][.]'
    return t

def t_WILDCARD(t):
    '[.][.]'
    return t

def t_PERIOD(t):
    '[.]'
    return t

def t_EQUALTO(t):
    '[=][=]'
    return t

def t_NOTEQUAL(t):
    '[!][=]'
    return t

def t_NOT(t):
    '[!]'
    return t

def t_LESSTHAN(t):
    '[<]'
    return t

def t_GREATERTHAN(t):
    '[>]'
    return t

def t_EQU(t):
    '[=]'
    return t

def t_WC(t):
    '[_]'
    return t

def t_LITCHAR(t):
    r'''[b][\'][\']*[\']|
    [b][\'][\\]*[\']|
    [b][\'].[\']'''
    return t

def t_LITSTRING(t):
    r'''[b][\"].[0-9a-zA-Z\"\\]*.[\"]|
    [b][\"].*.[\"]'''
    return t

def t_LITDEC(t):
    r"""
    [0-9]([0-9_]+)?
    """
    return t

def t_LITBOOL(t):
    r"\\btrue\\b|\\bfalse\\b"
    return t

def t_ID(t):
    r"""
    [a-zA-Z_]([_a-zA-Z0-9]+)?
    """
    return t

t_ignore = '\t '

def t_error(t):
    print("Illegal character '" + t.value[0] 
          + "' at line " + str(t.lexer.lineno))
    t.lexer.skip(1)

data = '''fn main() {
    // A simple integer calculator:
    // `+` or `-` means add or subtract by 1
    // `*` or `/` means multiply or divide by 2

    let program : &[u8] = b"++ ++ * -- /";
    let mut mp : [u8; 3] = [1, 2, 3];
    mp[1] = b'x';
    let mut accumulator = 0;

    let mut i = 0;
    while (i < program) {
        match (program[i]) {
            b'+' => {accumulator += 1;},
            b'-' => {accumulator -= 1;},
            b'*' => {accumulator *= 2;},
            b'/' => {accumulator /= 2;},
            _ => { /* ignore everything else */ }
        };
        i += 1;
    };

    putstr(b"Result: ");
    putint(accumulator);
}'''
    
lexer = lex.lex()

# lexer.input(data)

# while True:
#     tok = lexer.token()
#     if not tok: break
# #     if tok.type == 'ID' or tok.type == 'DECIMAL':
# #         print(tok.type + "(" + tok.value + ")")
# #     else:
#     print(tok.type + "(" + tok.value + ")")
    