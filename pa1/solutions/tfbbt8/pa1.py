# Terry Ballou-Crawford
# 14163546 TFBBT8
# 2/18/2015
# PA1: Python Lexer using PLY
# ****ONLY TESTED USING PYTHON 3.4****

import lex as lex
import sys

#List of token names
tokens = (
    'ID',
    'LITCHAR',
    'LITSTR',
    'LITBOOL',
    'OPERATOR',
    'SYMBOL',
    'PRIMITIVE_TYPE',
    'LITDEC'
)

keywords = ('abstract', 'alignof', 'as', 'be', 'box', 'break',
           'const', 'continue', 'crate', 'do', 'else', 'enum',
           'extern', 'final', 'fn', 'for', 'if', 'impl',
           'in', 'let', 'loop', 'macro', 'macro_rules',
           'match', 'mod', 'move', 'mut', 'offsetof', 'override',
           'priv', 'pub', 'pure', 'ref', 'return', 'sizeof', 'static', 
           'self', 'struct', 'super', 'trait', 'type', 'typeof', 
           'unsafe', 'unsized', 'use', 'virtual', 'where', 'while', 'yield',
           )

#Ignore comments
def t_COMMENT(t):
    r'''[//][//][//].*|
    [//][\*].*.[\*][//]|
    [//][//].*'''
    pass

#String
def t_LITSTR(t):
    r'''[b][\"].[0-9a-zA-Z\"\\]*.[\"]|
    [b][\"].*.[\"]'''
    value = list(t.value)
    newvalue = ""
    i = 0
    for c in value:
        if i != 0 and i != 1 and i != len(value)-1:
            newvalue = newvalue + c
        i = i + 1
    
    t.value = 'LITSTR(' + newvalue + ')'
    return t

#Boolean
def t_LITBOOL(t):
    r'[t][r][u][e]|[f][a][l][s][e]'
    t.value = 'LITBOOL(' + t.value + ')'
    return t

#Characters
def t_LITCHAR(t):
    r'''[b][\'][\']*[\']|
    [b][\'][\\]*[\']|
    [b][\'].[\']'''
    value = list(t.value)
    t.value = value[2]
    t.value = 'LITCHAR(' + t.value + ')'
    return t

def t_PRIMITIVE_TYPE(t):
    r'''
        ^[b][o][o][l]$|^[u][8]$|^[u][1][6]$|^[u][3][2]$|^[u][6][4]&|^[i][8]$|^[i][1][6]$|
        ^[i][3][2]$|^[i][6][4]$|^[f][3][2]$|^[f][6][4]$|^[u][s][i][z][e]$|^[i][s][i][z][e]$|
        ^[c][h][a][r]$|[s][t][r]
    '''
    return t

#ID tokens
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    for string in keywords:
        if t.value == string:
            return t
    t.value = 'ID(' + t.value + ')'
    return t

def t_LITDEC(t):
    r'\d+_\d+|\d+'
    newval = list(t.value)
    if '_' in t.value:
        newval.remove('_')
    newval = ''.join(newval)
    t.value = 'LITDEC(' + newval + ')'
    return t

#Newline character
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    pass
   
#Ignored characters 
t_ignore = '\t| '

def t_SYMBOL(t):
    r'''
        [:][:]|[-][>]|[=][>]|[#][!]|[#]|[']|[$]|[\[]|[\]]|[(]|[)]|[{]|[}]|[,]|[;]
    '''
    return t

def t_OPERATOR(t):
    r'''
        [<][<][=]
        |[>][>][=]|[.][.][.]|[.][.]|[<][<]|[>][>]|[|][|]|[&][&]|[!][=]|[<][=]|
        [>][=]|[+][=]|[-][=]|[*][=]|[/][=]|[%][=]|[&][=]|[|][=]|[\^][=]|[#][!]|[=][=]|
        [<]|[>]|[-]|[\*]|[=]|[!]|[+]|[/]|[%]|[&]|[|]|[\^]|[.]
    '''
    return t

#Character not recognized
def t_error(t):
    print("Illegal character '" + t.value[0] + "' at line " + str(t.lexer.lineno))
    t.lexer.skip(1)
    
lexer = lex.lex(debug=0)

while True:
    # Test it out
    prompt = input("Enter Rust code, q to exit, or press enter to run from file/hard-coded values: ")
    if prompt == 'q':
        sys.exit(1)
    if len(prompt) < 1:
        data = None
        if len(sys.argv) == 2:
            text_file = open(sys.argv[1], "r")
            data = text_file.read()
            text_file.close
        else:
            data = '''fn main() {
                // A simple integer calculator:
                // `+` or `-` means add or subtract by 1
                // `*` or `/` means multiply or divide by 2
            
                let program = b"+ + * - /";
                let mut accumulator = 0;
            
                for token in program.chars() {
                    match token {
                        b'+' => accumulator += 1,
                        b'-' => accumulator -= 1,
                        b'*' => accumulator *= 2,
                        b'/' => accumulator /= 2,
                        _ => { /* ignore everything else */ }
                    }
                }
            
                println!(b"The program \"{}\" calculates the value {}",
                          program, accumulator);
            }'''
        lexer.input(data)
    else:
        lexer.input(prompt)
    
    # Tokenize
    while True:
        tok = lexer.token()
        if not tok: break      # No more input
        print(tok.value)