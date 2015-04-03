#Terry Ballou-Crawford
#14163546
#3/16/15
#Programming Assignment 2

import yacc as yacc

#Get tokens from lexer
from lexer import tokens
import lexer as lexer
import sys

temp = []
thelist = []

outputFile = open(str(sys.argv[2]), 'w')

text_file = open(sys.argv[1], "r")
data = text_file.read()
text_file.close

def p_expression(p):
    '''expression : term expression
                | term COMMA expression
                | term OPERATOR expression
                | term SEMICOLON
                | term SINGLEOR expression
                | term SYMBOL expression
                | term COLON expression
                | expression COMMA expression
                | LBRACK expression RBRACK
                | '''
def p_term(p):
    '''term : LOOP LBRACK RBRACK
            | NIL
            | ID
            | TYPE
            | BOX LEQ litterm GEQ
            | BOX LEQ TYPE GEQ
            | BOX DOUBLECOLON NEW LPAREN litterm RPAREN
            | ID COLON litterm'''
    temp.append("(term: " + p[1])
    
def p_litterm_literals(p):
    '''litterm : BOOL
            | CHAR
            | STRING
            | NUMBER'''
    temp.append("(literal: " + p[1])

def p_term_fieldlookup(p):
    '''term : ID DOT expression
            | ID LPAREN RPAREN DOT expression'''
    thelist.append("(field-lookup, id: " + p[1])
    for item in temp:
        thelist.append(item)
    temp.clear()

def p_term_assignment(p):
    '''term : ID EQ expression
            | ID OPERATOR expression'''
    thelist.append("(assignment, id: " + p[1])
    for item in temp:
        thelist.append(item)
    temp.clear()

def p_term_while(p):
    '''term : WHILE LPAREN expression RPAREN LBRACK expression RBRACK'''
    thelist.append("(while-loop")
    for item in temp:
        thelist.append(item)
    temp.clear()
    
def p_term_loop(p):
    '''term : LOOP LBRACK expression RBRACK'''
    thelist.append("(loop-loop")
    for item in temp:
        thelist.append(item)
    temp.clear()

def p_term_if(p):
    '''term : IF LPAREN expression RPAREN LBRACK expression RBRACK ELSE LBRACK expression RBRACK
            | IF LPAREN expression RPAREN LBRACK expression RBRACK'''
    if len(p) > 7:
        thelist.append("(if-else")
    else:
        thelist.append("(if")
    for item in temp:
        thelist.append(item)
    temp.clear()
    
def p_term_match(p):
    '''term : MATCH LPAREN expression RPAREN LBRACK expression RBRACK'''
    thelist.append("(match")
    for item in temp:
        thelist.append(item)
    temp.clear() 

#FUNCTION CODE
def p_term_functiondef(p):
    '''term : FUNCTION ID LPAREN expression RPAREN RARROW TYPE LBRACK expression RBRACK
                | FUNCTION ID LPAREN expression RPAREN RARROW NOT LBRACK expression RBRACK
                | FUNCTION ID LPAREN expression RPAREN LBRACK expression RBRACK'''
    thelist.append("(fn-def, id: " + p[2])
    for item in temp:
        thelist.append(item)
    temp.clear()
     
def p_term_functionparams(p):
    '''term : ID COLON TYPE
            | ID COLON ID'''
    temp.append("(fn-params, id: " + p[1] + ", type: " + p[3])
    
def p_term_functioncall(p):
    '''term : ID LPAREN expression RPAREN'''
    thelist.append("(function-call")
    for item in temp:
        thelist.append(item)
    temp.clear()
    
#ENUM CODE
def p_term_enum(p):
    '''term : ENUM ID LBRACK expression RBRACK'''
    thelist.append("(enum-def, id " + p[2])
    for item in temp:
        thelist.append(item)
    temp.clear()

#STRUCT CODE
def p_term_struct(p):
    '''term : STRUCT ID LBRACK expression RBRACK'''
    thelist.append("(struct-def, id " + p[2])
    for item in temp:
        thelist.append(item)
    temp.clear()
    
#TYPE

#STATEMENT
def p_term_let(p):
    '''term : LET ID COLON TYPE EQ expression
            | LET ID DOUBLECOLON ID LPAREN expression RPAREN EQ expression
            | LET ID SEMICOLON
            | LET LSQBRACK expression RSQBRACK EQ expression'''
    if p[2] != '[':
        thelist.append("(let-statement, id " + p[2])
    else:
        thelist.append("(let-statement, array")
    for item in temp:
        thelist.append(item)
    temp.clear()
     
#RETURN
def p_term_return(p):
    '''term : RETURN expression SEMICOLON'''
    thelist.append("(return-statement")
    for item in temp:
        thelist.append(item)
    temp.clear()
    
#PATTERNS
def p_term_namebind(p):
    '''term : REF ID
            | REF MUT ID
            | MUT ID'''
    if p[1] != 'mut':
        thelist.append("(namebind, id: " + p[2])
    else:
        thelist.append("(namebind, id: " + p[3])
        
def p_term_deref(p):
    '''term : DEREF ID'''
    thelist.append("(deref, id: " + p[2])
    
def p_term_array(p):
    '''term : LSQBRACK expression RSQBRACK'''
    thelist.append("(array")
    
def p_term_enumpattern(p):
    '''term : ID DOUBLECOLON expression'''
    thelist.append("(enum-pattern, id: " + p[1])
    
def p_term_structpattern(p):
    '''term : ID LBRACK expression RBRACK'''
    thelist.append("(struct-pattern, id: " + p[1])
    
def p_error(p):
    global data
    print("Illegal token '" + str(p.value) + "' at line " + str(p.lineno)
          + " position " + str(find_column(data, p)))
    outputFile.write("Illegal token '" + str(p.value) + "' at line " + str(p.lineno)
          + " position " + str(find_column(data, p)) + '\n')
    
def find_column(theinput,token):
    last_cr = theinput.rfind('\n',0,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

def printAST():
    for item in thelist:
        outputFile.write(item + '\n')
        
# Build the parser
parser = yacc.yacc() 
result = parser.parse(data)
printAST()