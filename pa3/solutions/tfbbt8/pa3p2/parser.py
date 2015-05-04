#Terry Ballou-Crawford
#PA3 Pt. 1
#4/7/2015

import yacc as yacc
import sys as sys
from enum import Enum

#Get tokens from theLexer
from lexer import tokens
import lexer as lexer
from lib2to3.fixer_util import String
from setuptools.command.build_ext import if_dl
from _operator import itemgetter

class AST():
    def __init__(self,value,children):
        self.value = value
        self.children = children
        
class Node():
    def __init__(self,value,children):
        self.value = value
        self.children = children
        
precedence = (
    ('left', 'LOGICALOR'),  # Nonassociative operators
    ('left', 'LOGICALAND'),
    ('left', 'LESSTHAN', 'LESSTHANOREQUAL', 'GREATERTHAN', 'GREATERTHANOREQUAL', 'EQUALTO', 'NOTEQUAL'),
    ('right', 'NOT'),
    ('left', 'ADD', 'SUB'),
    ('left', 'MUL', 'DIV', 'MODOP')
)

listOfFunctions = []
listOfEnums = []
listOfStructs = []

start = 'crate'

ok = True

def p_crate(p):
    '''crate : items'''
    #'''{ print_AST( build_AST("crate", push(None, build_AST("items", $1))), 0 );'''
    if ok:
        crate = [AST("crate:ok!", [AST("items", [p[1]])])]
        #printAST(crate, 0)
        printLLVM(crate)
    else:
        crate = [AST("crate:ERROR!", [AST("items", [p[1]])])]
        printAST(crate, 0)
        printLLVMError()

def p_items(p):
    '''items : item items
            | item
            | '''
#     '''item items { $$ = push(None, $1); $$->next = $2;}
#         | item{ $$ = push(None, $1);}
#         | { $$ = None;};'''
    if len(p) == 3:
        p[0] = p[1]
        p[0].next = p[2]
    elif len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = None

def p_item(p):
    '''item : function
            | struct
            | enum'''
#     '''function { $$ = $1; }
#     | struct   { $$ = $1; }
#     | enum     { $$ = $1; };'''
    p[0] = p[1]
    
def p_stmts(p):
    '''stmts : stmt SEMICOLON stmts
            | stmt
            | '''
#     '''stmt SEMICOLON stmts { $$ = push(None, $1); $$->next = $3;}
#     | stmt                 { $$ = push(None, $1);  }
#     | { $$ = None;            };'''
    if len(p) == 4:
        p[0] = p[1]
        p[0].next = p[3]
    elif len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = None

def p_stmt(p):
    '''stmt : return
            | exp
            | let
            | function'''
#     '''return   { $$ = $1; } 
#     | exp      { $$ = $1; }
#     | let      { $$ = $1; }
#     | function { $$ = $1; };'''
    #p[0] = p[1]
    if ok:
        p[0] = AST("stmt-exp:()", [p[1]])
    else:
        p[0] = AST("stmt-exp:ERROR!", [p[1]])
    p[0].next = AST("unit:()", [None])
  
def p_exps_seq(p):
    '''exps : exps
            | '''
#     '''exps { $$ = $1;   }
#     | { $$ = None; };'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = None

def p_exps(p):
    '''exps : exp COMMA exps
            | exp'''
#     '''exp COMMA exps { $$ = push(None, $1); $$->next = $3; }
#     | exp { $$ = push(None, $1);};'''
    if len(p) == 4:
        p[0] = p[1]
        p[0].next = p[3]
    else:
        p[0] = None
    
def p_exp(p):
    '''exp : LPAREN exp RPAREN
        | arith1
        | loop
        | while
        | box_new
        | if
        | match
        | arg_struct
        | assign
        | arr
        | arr_index
        | unit
        | '''
#     '''LPAREN exp RPAREN { $$ = $2;}
#    | arith1 { $$ = $1;}
#    | loop { $$ = $1;}
#    | while { $$ = $1;}
#    | box_new { $$ = $1;}
#    | if { $$ = $1;}
#    | match { $$ = $1;}
#    | arg_struct { $$ = $1;}
#    | assign { $$ = $1;}
#    | arr { $$ = $1;}
#    | arr_index { $$ = $1;}
#    | unit { $$ = $1;}
#    | { $$ = None;};'''
    if len(p) == 4:
        p[0] = p[2]
    elif p[1]:
        p[0] = p[1]
    else:
        p[0] = None

def p_arith1(p):
    '''arith1 : add
            | sub
            | arith2'''
#     '''add { $$ = $1; }
#     | sub { $$ = $1; }
#     | arith2 { $$ = $1; };'''
    p[0] = p[1]
     
def p_arith2(p):
    '''arith2 : mul
            | div
            | mod
            | neg
            | arith3'''
#     '''mul           { $$ = $1; }
#     | div           { $$ = $1; }
#     | mod           { $$ = $1; }
#     | neg           { $$ = $1; }
#     | arith3        { $$ = $1; };'''
    p[0] = p[1]
      
def p_arith3(p):
    '''arith3 : LPAREN arith1 RPAREN
            | fn_call
            | comp
            | field_lookup
            | arith4'''
#     '''LPAREN arith1 RPAREN { $$ = $2; }
#     | fn_call              { $$ = $1; }
#     | comp                 { $$ = $1; }
#     | field_lookup         { $$ = $1; }
#     | arith4               { $$ = $1; };'''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_arith4(p):
    '''arith4 : id
            | ptr
            | lit'''
#     '''id { $$ = $1; }
#       | ptr { $$ = $1; }
#       | lit { $$ = $1; };'''
    if len(p) == 2:
        p[0] = p[1]
      
def checkType(tocheck):
    temp = Node(None, None)
    for item in tocheck:
        node = getTypeValue(item)
        if not node:
            continue
        if node.value > temp.value:
            temp.value = node.value
        if getTheType(temp.type) < getTheType(node.type):
            temp.type = node.type
    if temp.type:
        return temp.type + temp.value
    else:
        return None
        
def getTheType(string):
    if 'i' in string:
        return 2
    elif 'u' in string:
        return 1
    elif 'f' in string:
        return 3
         
         
def getTypeValue(string):
    node = Node(None, None)
    if not string:
        return None
    if 'i' in string:
        if 'eight' in string:
            node.type = 'i'
            node.value = 8
            return node
        elif 'sixteen' in string:
            node.type = 'i'
            node.value = 16
            return node
        elif 'threetwo' in string:
            node.type = 'i'
            node.value = 32
            return node
        elif 'sixfour' in string:
            node.type = 'i'
            node.value = 64
            return node
    elif 'u' in string:
        if 'eight' in string:
            node.type = 'u'
            node.value = 8
            return node
        elif 'sixteen' in string:
            node.type = 'u'
            node.value = 16
            return node
        elif 'threetwo' in string:
            node.type = 'u'
            node.value = 32
            return node
        elif 'sixfour' in string:
            node.type = 'u'
            node.value = 64
            return node
    elif 'f' in string:
        if 'threetwo' in string:
            node.type = 'f'
            node.value = 32
            return node
        elif 'sixfour' in string:
            node.type = 'f'
            node.value = 64
            return node
    else:
        return None
  
thetype = ""  
listoftypes = []
    
def p_add(p):
    '''add : arith1 ADD arith1'''
#     '''arith1 ADD arith1 { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("add", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    global thetype, listoftypes
    checkTypes(listoftypes)
    if p[1] and p[3]:
        p[0] = AST("add:" + thetype, newList)
    else:
        p[0] = AST("add:", newList)
    global ok
    if not ok:
        p[0] = AST("add:ERROR!", newList)
    listoftypes = []
    
def getEnum(string):
    if string == 'bool':
        return 'BOOL'
    if string == 'char':
        return 'CHAR'
    if string == 'string':
        return 'STR'
    if string == 'i32':
        return 'NUM'
 
def checkTypes(tocheck):
    storedtype = None
    for item in tocheck:
        if type(item) == list:
            checkTypes(item)
        elif type(item) == str:
            if not storedtype:
                storedtype = getEnum(item)
            elif storedtype != getEnum(item):
                global ok
                ok = False
                return
                
def p_sub(p):
    '''sub : arith1 SUB arith1'''
#     '''arith1 SUB arith1 { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("sub", l); }; '''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    global thetype
    if p[1] and p[3]:
        p[0] = AST("sub:" + thetype, newList)
    else:
        p[0] = AST("sub:", newList)
    thetype = ""

def p_mul(p):
    '''mul : arith1 MUL arith1'''
#     '''arith1 MUL arith1 { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("mul", l); }; '''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    global thetype
    if p[1] and p[3]:
        p[0] = AST("mul:" + thetype, newList)
    else:
        p[0] = AST("mul:", newList)
    thetype = ""

def p_div(p):
    '''div : arith1 DIV arith1'''
#     '''arith1 DIV arith1 { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("div", l); }; '''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    global thetype
    if p[1] and p[3]:
        p[0] = AST("div:" + thetype, newList)
    else:
        p[0] = AST("div:", newList)
    thetype = ""

def p_mod(p):
    '''mod : arith1 MODOP arith1'''
#     '''arith1 MODOP arith1 { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("rem", l); }; '''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    global thetype
    if p[1] and p[3]:
        p[0] = AST("rem:" + thetype, newList)
    else:
        p[0] = AST("rem:", newList)
    thetype = ""

def p_neg(p):
    '''neg : SUB arith4'''
#     '''SUB arith4 { $$ = build_AST("neg", push(None, $2)); };'''
    p[0] = AST("neg", [p[2]])

def p_comp(p): 
    '''comp : lt
            | leq
            | gt
            | geq
            | eq
            | neq
            | or
            | and
            | not'''
#     '''lt  { $$ = $1; }
#     | leq { $$ = $1; }
#     | gt  { $$ = $1; }
#     | geq { $$ = $1; }
#     | eq  { $$ = $1; }
#     | neq { $$ = $1; }
#     | or  { $$ = $1; }
#     | and { $$ = $1; }
#     | not { $$ = $1; };'''
    p[0] = p[1]

def p_lt(p):
    '''lt : arith1 LESSTHAN arith1'''
#     '''arith1 LESSTHAN arith1           { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("lt", l);  };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("lt", newList)
    
def p_leq(p): 
    '''leq : arith1 LESSTHANOREQUAL arith1'''
#     '''arith1 LESSTHANOREQUAL arith1    { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("leq", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("leq", newList)
    
def p_gt(p):  
    '''gt : arith1 GREATERTHAN arith1'''
#     '''arith1 GREATERTHAN arith1        { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("gt", l);  };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("gt", newList)
    
def p_geq(p): 
    '''geq : arith1 GREATERTHANOREQUAL arith1'''
#     '''arith1 GREATERTHANOREQUAL arith1 { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("geq", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("geq", newList)
    
def p_eq(p): 
    '''eq : arith1 EQUALTO arith1''' 
#     '''arith1 EQUALTO arith1            { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("eq", l);  };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("eq", newList)
    
def p_neq(p): 
    '''neq : arith1 NOTEQUAL arith1'''
#     '''arith1 NOTEQUAL arith1           { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("neq", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("neq", newList)
    
def p_or(p):
    '''or : arith1 LOGICALOR arith1'''
#     '''arith1 LOGICALOR arith1          { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("or", l);  };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("or", newList)
    
def p_and(p):
    '''and : arith1 LOGICALAND arith1'''
#     '''arith1 LOGICALAND arith1         { List * l; l = push(None, $1); l = push(l, $3); $$ = build_AST("and", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("and", newList)
    
def p_not(p):
    '''not : NOT arith1'''
#     '''NOT arith1                       { $$ = build_AST("not", push(None, $2));};'''
    p[0] = AST("not", [p[2]])

def p_ptr(p):
    '''ptr : addr
            | deref
            | addr_of_mut'''
#     '''addr        { $$ = $1; }
#    | deref       { $$ = $1; }
#    | addr_of_mut { $$ = $1; };'''
    p[0] = p[1]

def p_addr(p):
    '''addr : AND arith4
            | AND neg'''
#     '''AND arith4 { $$ = build_AST("addr-of", push(None, $2));     }
#     | AND neg { $$ = build_AST("addr-of", push(None, $2));     };'''
    p[0] = AST("addr-of", [p[2]])
    
def p_deref(p):      
    '''deref : MUL arith4
            | MUL neg'''
#     '''MUL arith4     { $$ = build_AST("deref", push(None, $2));       }
#      | MUL neg        { $$ = build_AST("deref-of", push(None, $2));    };'''
    p[0] = AST("deref", [p[2]])
     
def p_addr_of_mut(p):
    '''addr_of_mut : AND MUT arith4
                    | AND MUT neg'''
#     '''AND MUT arith4 { $$ = build_AST("addr-of-mut", push(None, $3)); }
#     | AND MUT neg    { $$ = build_AST("addr-of-mut", push(None, $3));    };'''
    p[0] = AST("addr-of-mut", [p[3]])

def p_unit(p):
    '''unit : LPAREN RPAREN'''
#     '''LPAREN RPAREN { $$ = build_AST("unit", None); };'''
    p[0] = AST("unit", [None])

def p_assign(p):
    '''assign : exp EQU exp
            | exp PLUSEQUAL exp
            | exp SUBEQUAL exp
            | exp DIVEQUAL exp
            | exp TIMESEQUAL exp
            | exp MODEQUAL exp
            | exp ANDEQUAL exp
            | exp OREQUAL exp'''
#     '''exp EQU exp        { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign", l);     }
#       | exp PLUSEQUAL exp  { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign-add", l); }
#       | exp SUBEQUAL exp   { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign-sub", l); }
#       | exp DIVEQUAL exp   { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign-div", l); }
#       | exp TIMESEQUAL exp { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign-mul", l); }
#       | exp MODEQUAL exp   { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign-rem", l); }
#       | exp ANDEQUAL exp   { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign-and", l); }
#       | exp OREQUAL exp    { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("assign-or", l);  };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    if p[2] == 'EQU':
        p[0] = AST("assign", newList)
    elif p[2] == 'PLUSEQUAL':
        p[0] = AST("assign-add", newList)
    elif p[2] == 'SUBEQUAL':
        p[0] = AST("assign-sub", newList)
    elif p[2] == 'DIVEQUAL':
        p[0] = AST("assign-div", newList)
    elif p[2] == 'TIMESEQUAL':
        p[0] = AST("assign-mul", newList)
    elif p[2] == 'MODEQUAL':
        p[0] = AST("assign-rem", newList)
    elif p[2] == 'ANDEQUAL':
        p[0] = AST("assign-and", newList)
    elif p[2] == 'OREQUAL':
        p[0] = AST("assign-or", newList)
    

def p_let(p): 
    '''let : LET pat COLON type EQU exp
            | LET pat COLON type
            | LET pat EQU exp
            | LET pat'''
#     '''LET pat COLON type EQU exp { List *l; l = push(None, $2); l = push(l, $4); l = push(l, $6); $$ = build_AST("let", l);}
#     | LET pat COLON type         { List *l; l = push(None, $2); l = push(l, $4); $$ = build_AST("let", l); }
#     | LET pat EQU exp            { List *l; l = push(None, $2); l = push(l, $4); $$ = build_AST("let", l); }
#     | LET pat                    { $$ = build_AST("let", push(None, $2));     };'''
    newList = []
    newList.append(p[2])
    newList.append(p[4])
    p[0] = AST("let", newList)
    if len(p) == 7:
        newList.append(p[6])
        p[0] = AST("let", newList)
    elif len(p) == 3:
        p[0] = AST("let", [p[2]])

def p_field_lookup(p): 
    '''field_lookup : exp PERIOD id'''
#     '''exp PERIOD id { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("field-lookup", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("field-lookup", newList)

def p_while(p): 
    '''while : WHILE LPAREN exp RPAREN block'''
#     '''WHILE LPAREN exp RPAREN block { List *l; l = push(None, $3); l = push(l, $5); $$ = build_AST("while", l); };'''
    newList = []
    newList.append(p[3])
    newList.append(p[5])
    p[0] = AST("while", newList)

def p_loop(p):
    '''loop : LOOP block'''
#     '''LOOP block { $$ = build_AST("loop", push(None, $2)); };'''
    p[0] = AST("loop", [p[2]])

def p_if(p):
    '''if : IF LPAREN exp RPAREN block
        | if ELSE block'''
#     '''IF LPAREN exp RPAREN block { List *l; l = push(None, $3); l = push(l, $5); $$ = build_AST("if", l); }
#     | if ELSE block              { $1->children = push($1->children, $3); $$ = $1;                        };'''
    if len(p) == 6:
        newList = []
        newList.append(p[3])
        newList.append(p[5])
        p[0] = AST("if", newList)
    else:
        p[1].children.append(p[3])
        p[0] = p[1]

def p_match(p): 
    '''match : MATCH LPAREN exp RPAREN LSQUIGBRACKET match_arms RSQUIGBRACKET'''
#     '''MATCH LPAREN exp RPAREN LSQUIGBRACKET match_arms RSQUIGBRACKET {
#          List *l; l = push(None, $3); l = push(l, build_AST("match-arms", $6)); ;
#          $$ = build_AST("match", l);                                  };'''
    newList = []
    newList.append(p[3])
    newList.append(AST("match-arms", [p[6]]))
    p[0] = AST("match", newList) 

def p_match_arms(p): 
    '''match_arms : match_arm COMMA match_arms
            | match_arm'''
#     '''match_arm COMMA match_arms { $$ = push(None, $1); $$->next = $3; }
#           | match_arm                  { $$ = push(None, $1);                };'''
    if len(p) == 4:
        p[0] = p[1]
        p[0].next = p[3]
    else:
        p[0] = p[1]

def p_match_arm(p): 
    '''match_arm : match_pats PIPE block'''
#     '''match_pats PIPE block{ List *l; l = push(None, build_AST("pats", $1)); 
#                                   l = push(l, $3); $$ = build_AST("match-arm", l);     };'''
    newList = []
    newList.append(AST("pats", [p[1]]))
    newList.append(p[3])
    p[0] = AST("match-arm", newList)

def p_match_pats(p): 
    '''match_pats : pat OR match_pats
                | pat'''
#     '''pat OR match_pats { $$ = push(None, $1); $$->next = $3; }
#           | pat               { $$ = push(None, $1);                };'''
    if len(p) == 4:
        p[0] = p[1]
        p[0].next = p[3]
    else:
        p[0] = p[1]
          
def p_pats(p): 
    '''pats : pat COMMA pats
            | pat'''
#     '''pat COMMA pats { $$ = push(None, $1); $$->next = $3; }
#           | pat               { $$ = push(None, $1);                };'''
    if len(p) == 4:
        p[0] = p[1]
        p[0].next = p[3]
    else:
        p[0] = p[1]
          
def p_pat(p):
    '''pat : p_lit
            | LPAREN RPAREN
            | WC
            | SUB p_lit
            | id
            | REF id
            | REF MUT id
            | MUT id
            | AND pat
            | pat_arr
            | pat_enum
            | pat_struct'''
#     '''p_lit         { $$ = build_AST("pat-lit", push(None, $1));        }
#    | LPAREN RPAREN { $$ = build_AST("pat-unit", None);                 }
#    | WC            { $$ = build_AST("pat-wild", None);                 }
#    | SUB p_lit     { $$ = build_AST("pat-lit", push(None, $2));        }
#    | id            { $$ = build_AST("pat-id", push(None, $1));         }
#    | REF id        { $$ = build_AST("pat-ref-id", push(None, $2));     }
#    | REF MUT id    { $$ = build_AST("pat-ref-mut-id", push(None, $3)); }
#    | MUT id        { $$ = build_AST("pat-mut-id", push(None, $2));     }
#    | AND pat       { $$ = build_AST("pat-deref", push(None, $2));      }
#    | pat_arr       { $$ = $1;                                          }
#    | pat_enum      { $$ = $1;                                          }
#    | pat_struct    { $$ = $1;                                          };'''
    if p[1] == 'p_lit':
        p[0] = AST("pat-lit", [p[1]])
    elif p[1] == 'LPAREN':
        p[0] = AST("pat-unit", [None])
    elif p[1] == 'WC':
        p[0] = AST("pat-wild", [None])
    elif p[1] == 'SUB':
        p[0] = AST("pat-lit", [p[2]])
    elif p[1] == 'id':
        p[0] = AST("pat-id", [p[1]])
    elif p[1] == 'REF' and p[2] == 'id':
        p[0] = AST("pat-ref-id", [p[2]])
    elif len(p) >= 3 and p[2] == 'MUT':
        p[0] = AST("pat-ref-mut-id", [p[3]])
    elif p[1] == 'MUT':
        p[0] = AST("pat-mut-id", [p[2]])
    elif p[1] == 'AND':
        p[0] = AST("pat-deref", [p[2]])
    else:
        p[0] = p[1] 
   
def p_pat_arr(p): 
    '''pat_arr : LBRACKET pats RBRACKET'''
#     '''LBRACKET pats RBRACKET { $$ = build_AST("pat-arr", push(None, build_AST("pat-arr-elems", $2))); };'''
    p[0] = AST("pat-arr", [AST("pat-arr-elems", [p[2]])])

def p_pat_struct(p): 
    '''pat_struct : id LSQUIGBRACKET pat_fields RSQUIGBRACKET'''
#     '''id LSQUIGBRACKET pat_fields RSQUIGBRACKET { List *l; l = push(None, $1); l = push(l, build_AST("pat-fields", $3));
#                                                         $$ = build_AST("pat-struct", l);                                       };'''
    newList = []
    newList.append(p[1])
    newList.append(AST("pat-fields", [p[3]]))
    p[0] = AST("pat-struct", newList)

def p_pat_fields(p):
    '''pat_fields : pat_field COMMA pat_fields
                | pat_field'''
#     '''pat_field COMMA pat_fields { $$ = push(None, $1); $$->next = $3; }
#           | pat_field                  {$$ = push(None, $1);                 };'''
    if len(p) == 4:
        p[0] = p[1]
        p[0].next = p[3]
    else:
        p[0] = None

def p_pat_field(p): 
    '''pat_field : id COLON pat'''
#     '''id COLON pat { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("pat-field", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("pat-field", newList)
   
def p_arr(p): 
    '''arr : LBRACKET exps RBRACKET'''
#     '''LBRACKET exps RBRACKET { $$ = build_AST("arr", push(None, build_AST("exprs", $2))); };   '''
    p[0] = AST("arr", [AST("exprs", [p[2]])])
  
def p_arr_index(p): 
    '''arr_index : exp LBRACKET exp RBRACKET'''
#     '''exp LBRACKET exp RBRACKET { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("arr-index", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("arr-index", newList)

def p_function(p): 
    '''function : FN id LPAREN fn_params_seq RPAREN block
                | FN id LPAREN fn_params_seq RPAREN OBJECTACCESS type block'''
#     '''FN id LPAREN fn_params_seq RPAREN block                   {
#             List *l; l = push(None, $2);
#             if ($4) l = push(l, build_AST("fn-params", $4));
#             l = push(l, $6);
#             $$ = build_AST("fn-def", l);                        }
#         | FN id LPAREN fn_params_seq RPAREN OBJECTACCESS type block {
#             List *l; l = push(None, $2);
#             if ($4) l = push(l, build_AST("fn-params", $4));
#             if ($7) l = push(l, $7);
#             l = push(l, $8);
#             $$ = build_AST("fn-def", l);                        };'''
    if len(p) != 9:
        newList = []
        newList.append(p[2])
        if p[4]:
            newList.append(AST("fn-params", [p[4]]))   
        else:
            newList.append(AST("type-unit", [None]))       
        newList.append(p[6])
        if ok:
            p[0] = AST("fn-def:ok!", newList)
        else:
            p[0] = AST("fn-def:ERROR!", newList)
        #addToFunctionList(listOfFunctions, "function", p[2].children.value, p[4], None)
    else:
        newList = []
        newList.append(p[2])
        if p[4]:
            newList.append(AST("fn-params", [p[4]])) 
        if p[7]:
            newList.append(p[7])
        newList.append(p[8])
        if ok:
            p[0] = AST("fn-def:ok!", newList)
        else:
            p[0] = AST("fn-def:ERROR!", newList)
        #addToFunctionList(listOfFunctions, "function", p[2].children.value, p[4], p[7])

def addToStructList(whichList, whichType, node, arguments, returntype):
    children = []
    while arguments:
        if hasattr(arguments, 'children'):
            l = arguments.children 
        else:
            break
        for s in l:
            if hasattr(s, 'value'):
                if s.value == 'id':
                    #print(s.children.value)
                    if s.children.value in children:
                            #outputFile.write("Error: duplicate declaration of the field '" + s.children.value +
                                  #"' of the struct '" + node + "'.\n")
                            sys.exit(-1)
                    else:
                        children.append(s.children.value)
        if hasattr(arguments, 'next'):
            arguments = arguments.next
        else:
            break
    for item in whichList:
        if item.value == node:
            #outputFile.write("Error: duplicate definition of struct or " + whichType + " '" + node + "'.\n")
            sys.exit(-1)
    for item in listOfEnums:
        if item.value == node:
            #outputFile.write("Error: duplicate definition of struct or " + whichType + " '" + node + "'.\n")
            sys.exit(-1)
    whichList.append(AST(node, children))

def addToEnumList(whichList, whichType, node, arguments, returntype):
    children = []
    while arguments:
        if arguments:
            l = arguments.children
            if type(l) is not AST:
                for s in l:
                    if hasattr(s, 'value'):
                        if s.value == 'id':
                            #print(s.children.value)
                            if s.children.value in children:
                                #outputFile.write("Error: duplicate definition of the constructor '" + s.children.value +
                                      #"' for the enum '" + node + "'.\n")
                                sys.exit(-1)
                            else:
                                children.append(s.children.value)
            else:
                if hasattr(l, 'value'):
                        if l.value == 'id':
                            #print(l.children.value)
                            if l.children.value in children:
                                #outputFile.write("Error: duplicate definition of the constructor '" + l.children.value +
                                      #"' for the enum '" + node + "'.\n")
                                sys.exit(-1)
                            else:
                                children.append(l.children.value)
            if hasattr(arguments, 'next'):
                arguments = arguments.next
            else:
                break
        else:
            break
    for item in whichList:
        if item.value == node:
            #outputFile.write("Error: duplicate definition of struct or " + whichType + " '" + node + "'.\n")
            sys.exit(-1)
    for item in listOfEnums:
        if item.value == node:
            #outputFile.write("Error: duplicate definition of struct or " + whichType + " '" + node + "'.\n")
            sys.exit(-1)
    whichList.append(AST(node, children))

def addToFunctionList(whichList, whichType, node, arguments, returntype):
    children = []
    while arguments:
        if arguments:
            l = arguments.children
            for s in l:
                if hasattr(s, 'value'):
                    if s.value == 'id':
                        #print(s.children.value)
                        if s.children.value in children:
                            #outputFile.write("Error: identifier '" + s.children.value + "' is bound more than once in the parameter list for the " + 
                                             #whichType + " '" + node + "'.\n")
                            sys.exit(-1)
                        else:
                            children.append(s.children.value)
            if hasattr(arguments, 'next'):
                arguments = arguments.next
            else:
                break
        else:
            break
    for item in whichList:
        if item.value == node:
            #outputFile.write("Error: duplicate definition of " + whichType + " '" + node + "'.\n")
            sys.exit(-1)
    if node == 'main' and returntype:
        #outputFile.write("Error: main function has the wrong type.\n")
        sys.exit(-1)
    whichList.append(AST(node, children))

def p_fn_call(p):
    '''fn_call : id LPAREN fn_args RPAREN''' 
#     '''id LPAREN fn_args RPAREN { List *l; l = push(None, $1);
#                                     if ($3) l = push(l, build_AST("exprs", $3));
#                                     else l = push(l, build_AST("", None));
#                                     $$ = build_AST("fn-call", l);                };'''
    newList = []
    newList.append(p[1])
    if p[3]:
        newList.append(AST("exprs", [p[3]]))
    else:
        newList.append(AST("", [None]))
    p[0] = AST("fn-call", newList)

def p_fn_args(p): 
    '''fn_args : fn_arg fn_args
                | '''
#     '''fn_arg fn_args { $$ = push(None, $1);
#                           $$->next = $2;       }
#     |                { $$ = None;           };'''
    if len(p) == 3:
        p[0] = p[1]
        p[0].next = p[2]
    else:
        p[0] = None

def p_fn_arg(p): 
    '''fn_arg : fn_arg COMMA
            | lit
            | id
            | box_new
            | fn_call
            | arg_struct'''
#     '''fn_arg COMMA { $$ = $1; }
#     | lit        { $$ = $1;                  }
#     | id         { $$ = $1;                  }
#     | box_new    { $$ = $1;                  }
#     | fn_call    { $$ = $1;                  }
#     | arg_struct {$$ = $1;                   };'''
    p[0] = p[1]
  
def p_lit(p): 
    '''lit : p_lit
            | enum_con'''
#     '''p_lit    { $$ = $1; }
#    | enum_con { $$ = $1; };'''
    p[0] = p[1]
  
def p_p_lit(p): 
    '''p_lit : LITDEC
            | LITCHAR
            | LITBOOL
            | LITSTRING'''
#     '''LITDEC    { $$ = build_AST("lit-dec", None);  }
#     | LITCHAR   { $$ = build_AST("lit-char", None); }
#     | LITBOOL   { $$ = build_AST($1, None);         }
#     | LITSTRING { $$ = build_AST("lit-str", None);  };'''
    mytype = getLitType(p[1])
    global thetype, listoftypes
    if mytype == 'int':
        p[0] = AST("lit-dec:i32", [None])
        listoftypes.append("i32")
        thetype = "i32"
    elif mytype == 'char':
        p[0] = AST("lit-char", [None])
        listoftypes.append("char")
        thetype = "char"
    elif mytype == 'bool':
        p[0] = AST(p[1] + ":bool", [None])
        listoftypes.append("bool")
        thetype = "bool"
    elif mytype == 'string':
        p[0] = AST('lit-str', [None])
        listoftypes.append("string")
        thetype = "string"
   
def getLitType(string):
    try:
        int(string)
        return "int"
    except:
        pass
    if string == "true" or string == "false":
        return "bool"
    if r'b"' in string:
        return "char"
        
def p_id(p): 
    '''id : ID'''
#     '''ID {
#     $$ = build_AST("id", push(None, build_AST($1, None))) ;};'''
    p[0] = AST("id", [AST(p[1], [None])])

def p_fn_params_seq(p): 
    '''fn_params_seq : fn_params
                    | '''
#     '''fn_params { $$ = $1;   }
#              |           { $$ = None; };'''
    if len(p) > 1:
        p[0] = p[1]
    else:
        p[0] = None

def p_fn_params(p): 
    '''fn_params : fn_param COMMA fn_params
                | fn_param'''
#     '''fn_param COMMA fn_params { $$ = push(None, $1); $$->next = $3; }
#         | fn_param                 { $$ = push(None, $1);                };'''
    if len(p) == 4:
        p[0] = p[1]
        p[0].next = p[3]
    else:
        p[0] = p[1]
          
def p_fn_param(p): 
    '''fn_param : pat COLON type
                | pat COLON id'''
#     '''pat COLON type { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("fn-param", l); }
#         | pat COLON id   { List *l; l = push(None, $1); l = push(l, $3); $$ = build_AST("fn-param", l); };'''
    newList = []
    newList.append(p[1])
    newList.append(p[3])
    p[0] = AST("fn-param", newList)

def p_types(p): 
    '''types : type COMMA types
            | type'''
#     '''type COMMA types { $$ = push(None, $1); $$->next = $3; }
#      | type             { $$ = push(None, $1);                }'''
    if len(p) == 4: 
        p[0] = p[1]
        p[0].next = p[3]
    else:
        p[0] = p[1]

def p_type(p): 
    '''type : BOOL
            | UEIGHT
            | USIXTEEN
            | UTHREETWO
            | USIXFOUR
            | IEIGHT
            | ISIXTEEN
            | ITHREETWO
            | ISIXFOUR
            | FTHREETWO
            | FSIXFOUR
            | CHAR
            | STR
            | box
            | NOT
            | type_ref
            | id
            | AND MUT type
            | type_arr
            | LPAREN RPAREN
            | '''
#     '''BOOL         { $$ = build_AST("type-bool", None);              }
#     | UEIGHT       { $$ = build_AST("type-u8", None);                }
#     | USIXTEEN     { $$ = build_AST("type-u16", None);               }
#     | UTHREETWO    { $$ = build_AST("type-u32", None);               }
#     | USIXFOUR     { $$ = build_AST("type-u64", None);               }
#     | IEIGHT       { $$ = build_AST("type-i8", None);                }
#     | ISIXTEEN     { $$ = build_AST("type-i16", None);               }
#     | ITHREETWO    { $$ = build_AST("type-i32", None);               }
#     | ISIXFOUR     { $$ = build_AST("type-i64", None);               }
#     | FTHREETWO    { $$ = build_AST("type-f32", None);               }
#     | FSIXFOUR     { $$ = build_AST("type-f64", None);               }
#     | CHAR         { $$ = build_AST("type-char", None);              }
#     | STR          { $$ = build_AST("type-string", None);            }
#     | box          { $$ = $1;                                        }
#     | NOT          { $$ = None;                                      }
#     | type_ref     { $$ = $1;                                        }
#     | id           { $$ = $1;                                        }
#     | AND MUT type { $$ = build_AST("type-ref-mut", push(None, $3)); }
#     | type_arr     { $$ = $1;                                        }
#     | LPAREN RPAREN{ $$ = build_AST("type-unit", None);              }
#     |              { $$ = None;                                      };'''    
    if p[1] == 'BOOL':
        thetype = p[1] + " terry"
        p[0] = AST("type-bool", [None])
    elif p[1] == 'UEIGHT':
        thetype = "u8"
        p[0] = AST("type-u8", [None])
    elif p[1] == 'USIXTEEN':
        thetype = "u16"
        p[0] = AST("type-u16", [None])
    elif p[1] == 'UTHREETWO':
        thetype = "u32"
        p[0] = AST("type-u32", [None])
    elif p[1] == 'USIXFOUR':
        thetype = "u64"
        p[0] = AST("type-u64", [None])
    elif p[1] == 'IEIGHT':
        thetype = "i8"
        p[0] = AST("type-i8", [None])
    elif p[1] == 'ISIXTEEN':
        thetype = "i16"
        p[0] = AST("type-i16", [None])
    elif p[1] == 'ITHREETWO':
        thetype = "i32"
        p[0] = AST("type-i32", [None])
    elif p[1] == 'ISIXFOUR':
        thetype = "i64"
        p[0] = AST("type-i64", [None])
    elif p[1] == 'FTHREETWO':
        thetype = "f32"
        p[0] = AST("type-f32", [None])
    elif p[1] == 'FSIXFOUR':
        thetype = "f64"
        p[0] = AST("type-f64", [None])
    elif p[1] == 'CHAR':
        thetype = "charterry"
        p[0] = AST("type-char", [None])
    elif p[1] == 'STR':
        thetype = "strterry"
        p[0] = AST("type-string", [None])
    elif p[1] == "(":
        p[0] = AST("type-unit", [None])
    elif p[1] == 'AND':
        p[0] = p[1]
    elif p[1] == 'NOT':
        p[0] = None
    elif not p[1]:
        p[0] = None
    else:
        p[0] = p[1]
    
def p_type_arr(p): 
    '''type_arr : LBRACKET type RBRACKET
                | LBRACKET type SEMICOLON p_lit RBRACKET'''
#     '''LBRACKET type RBRACKET                { $$ = build_AST("type-arr", push(None, $2));   }
#         | LBRACKET type SEMICOLON p_lit RBRACKET { List *l; l = push(None, $2); l = push(l, $4);
#                                                   $$ = build_AST("type-arr", l);                };'''
    if len(p) != 5:
        p[0] = AST("type-arr", [p[2]])
    else:
        newList = []
        newList.append(p[2])
        newList.append(p[4])
        p[0] = AST("type-arr", newList)
                                                  
def p_type_ref(p): 
    '''type_ref : AND type'''
#     '''AND type {$$ = build_AST("type-ref", push(None, $2)); };'''
    p[0] = AST("type-ref", [p[2]])

def p_box(p): 
    '''box : BOX LESSTHAN type GREATERTHAN'''
#     '''BOX LESSTHAN type GREATERTHAN { $$ = build_AST("type-box", push(None, $3)); };'''
    p[0] = AST("type-box", [p[3]])

def p_box_new(p): 
    '''box_new : BOX CLASSACCESS NEW LPAREN exp RPAREN'''
#     '''BOX CLASSACCESS NEW LPAREN exp RPAREN { $$ = build_AST("box-new", push(None, build_AST("exprs", push(None, $5)))); };'''
    p[0] = AST("box-new", [AST("exprs", [p[5]])])

def p_struct(p): 
    '''struct : STRUCT id LSQUIGBRACKET field_defs RSQUIGBRACKET'''
#     '''STRUCT id LSQUIGBRACKET field_defs RSQUIGBRACKET { List *l; l = push(None, $2); l = push(l, build_AST("field-defs", $4));
#                                                             $$ = build_AST("struct-def", l);                              };'''
    newList = []
    newList.append(p[2])
    newList.append(AST("field-defs", [p[4]]))
    p[0] = AST("struct-def", newList)
    #addToStructList(listOfStructs, "struct", p[2].children.value, p[4], None)
                           
def p_field_defs(p): 
    '''field_defs : field_def field_defs
                | field_def'''
#     '''field_def field_defs { $$ = push(None, $1); $$->next = $2; }
#         | field_def            { $$ = push(None, $1);                };'''
    if len(p) == 3:
        p[0] = p[1]
        p[0].next = p[2]
    else:
        p[0] = p[1]
    
          
def p_field_def(p): 
    '''field_def : field_def COMMA
                | id COLON type'''
#     '''field_def COMMA { $$ = $1;                         }
#         | id COLON type   { List *l; 
#                              l = push(None, $1); 
#                              l = push(l, $3); 
#                              $$ = build_AST("field-def", l);  };'''
    if len(p) != 4:
        p[0] = p[1]
    else:
        newList = []
        newList.append(p[1])
        newList.append(p[3])
        p[0] = AST("field-def", newList)

def p_block(p): 
    '''block : LSQUIGBRACKET stmts RSQUIGBRACKET'''
#     '''LSQUIGBRACKET stmts RSQUIGBRACKET { $$ = build_AST("block", $2); };'''
    global ok, thetype
    if ok:
        if len(thetype) > 0 and thetype:
            p[0] = AST("block:" + thetype, [p[2]])
        else:
            p[0] = AST("block:()", [p[2]])
    else:
        p[0] = AST("block:ERROR!", [p[2]])
    thetype = ""

def p_return(p): 
    '''return : RETURN
            | return exp'''
#     '''RETURN     { $$ = build_AST("return", None);           }
#       | return exp { $$ = build_AST("return", push(None, $2)); };'''
    if len(p) != 3:
        p[0] = AST("return", [None])
    else:
        p[0] = AST("return", [p[2]])

def p_arg_struct(p): 
    '''arg_struct : id LSQUIGBRACKET body RSQUIGBRACKET'''
#     '''id LSQUIGBRACKET body RSQUIGBRACKET { List* l; l = push(None, $1); l = push(l, build_AST("field-inits", $3));
#         $$ = build_AST("struct", l);}'''
    newList = []
    newList.append(p[1])
    newList.append(AST("field-inits", [p[3]]))
    p[0] = AST("struct", newList)

def p_body(p): 
    '''body : param_arg body
            | param_arg'''
#     '''param_arg body {$$ = push(None, $1); $$->next = $2; }
#     | param_arg      {$$ = push(None, $1);                }'''
    if len(p) == 3: 
        p[0] = p[1]
        p[0].next = p[2]
    else:
        p[0] = None

def p_param_arg(p):
    '''param_arg : param_arg COMMA
            | id COLON exp'''
#     '''param_arg COMMA {$$ = $1; }
#     | id COLON exp {List* l; l = push(None, $1); l = push(l, $3); $$ = build_AST("field-init", l);}'''
    if len(p) != 4:
        p[0] = p[1]
    else:
        newList = []
        newList.append(p[1])
        newList.append(p[3])
        p[0] = AST("field-init", newList)
    
def p_enum_con(p): 
    '''enum_con : enum_con LPAREN exps RPAREN
                | id CLASSACCESS id'''
#     '''enum_con LPAREN exps RPAREN   { $$->children = push($$->children, build_AST("exprs", $3)); }
#         | id CLASSACCESS id             { List *l; l = push(None, $1); l = push(l, $3);
#                                           $$ = build_AST("enum", push(None, build_AST("enum-ctor", l)));};'''
    if len(p) == 5:
        p[0].children.append(AST("exprs", [p[3]]))
    else:
        newList = []
        newList.append(p[1])
        newList.append(p[3])
        p[0] = AST("enum", [AST("enum-ctor", newList)])
        
def p_pat_enum(p):
    '''pat_enum : pat_enum LPAREN pats RPAREN
                | id CLASSACCESS id'''
#     '''pat_enum LPAREN pats RPAREN   { $$->children = push($$->children, build_AST("pat-enum-ctor-params", $3)); }
#         | id CLASSACCESS id             { List *l; l = push(None, $1); l = push(l, $3);
#                                           $$ = build_AST("pat-enum", push(None, build_AST("enum-ctor", l)));                            };'''
    if len(p) == 5:
        p[0].children.append(AST("pat-enum-ctor-params", [p[3]]))
    else:
        newList = []
        newList.append(p[1])
        newList.append(newList, p[3])
        p[0] = AST("pat-enum", [AST("enum-ctor", newList)])
                                          
def p_enum(p): 
    '''enum : ENUM id LSQUIGBRACKET enum_params RSQUIGBRACKET'''
#     '''ENUM id LSQUIGBRACKET enum_params RSQUIGBRACKET {
#         List* l; l = push(None, $2);
#         if($4) l = push(l, build_AST("enum-ctor-defs", $4));
#         $$ = build_AST("enum-def", l);};'''
    newList = []
    newList.append(p[2])
    if p[4]:
        newList.append(AST("enum-ctor-defs", [p[4]]))
    p[0] = AST("enum-def", newList)
    #addToEnumList(listOfEnums, "enum", p[2].children.value, p[4], None)

def p_enum_params(p):
    '''enum_params : enum_param enum_params
                    | '''
#     '''enum_param enum_params {$$ = push(None, $1);
#         $$->next = $2; };
#         | {$$ = None;};'''
    if len(p) == 3:
        p[0] = p[1]
        p[0].next = p[2]
    else:
        p[0] = None

def p_enum_param(p):
    '''enum_param : enum_param COMMA
                | id LPAREN types RPAREN
                | id'''
#     '''enum_param COMMA {$$ = $1;}
#         | enum_param LPAREN types RPAREN { $$->children = push($$->children, build_AST("enum-ctor-params", $3)); }
#         | id {$$ = build_AST("enum-ctor-def", push(None, $1));};'''
    if len(p) == 3:
        p[0] = p[1]
    elif len(p) == 5:
        p[0] = AST("enum-ctor-def", [p[1]])
        p[0].children.append(AST("enum-ctor-params", [p[3]]))
    else:
        p[0] = AST("enum-ctor-def", [p[1]])
    
def p_error(p):
    global data
    print("Illegal token '" + str(p.value) + "' at line " + str(p.lineno)
          + " position " + str(find_column(data, p)))
    
def find_column(theinput,token):
    last_cr = theinput.rfind('\n',0,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column

# def printAST(node, depth):
#     if not node:
#         return
#     if hasattr(node, 'value'):
#         if not node.value:
#             sys.stdout.write("ERROR")
#             return
#     for num in range(0, depth):
#         sys.stdout.write("\t")
#     if hasattr(node, 'value'):
#         sys.stdout.write(node.value)
#     if hasattr(node, "children") and node.children:
#         while node.children:
#             sys.stdout.write("\n")
#             printAST(node.children, depth + 1)
#             if hasattr(node.children, "next"):
#                 node.children = node.children.next
#             else:
#                 return
#     else:
#         return  
    
# def printASTtoFile(node, depth):
#     if type(node) == list:
#         for thing in node:
#             if thing:
#                 for num in range(0, depth):
#                     outputFile.write("\t")
#                 if hasattr(thing, 'value'):
#                     outputFile.write("(" + thing.value)
#                 else:
#                     outputFile.write("(" + thing)
#                 outputFile.write("\n")
#                 if hasattr(thing, "children"):
#                     printAST(thing.children, depth + 1)
#                 if hasattr(thing, "next"):
#                     printAST([thing.next], depth)
#     elif type(node) == str:
#         for num in range(0, depth):
#             outputFile.write("\t")
#         outputFile.write(str)

def printAST(node, depth):
    if type(node) == list:
        for thing in node:
            if thing:
                for num in range(0, depth):
                    sys.stdout.write("\t")
                if hasattr(thing, 'value'):
                    sys.stdout.write("(" + thing.value)
                else:
                    sys.stdout.write("(" + thing)
                sys.stdout.write("\n")
                if hasattr(thing, "children"):
                    printAST(thing.children, depth + 1)
                if hasattr(thing, "next"):
                    printAST([thing.next], depth)
    elif type(node) == str:
        for num in range(0, depth):
            sys.stdout.write("\t")
        sys.stdout.write(str)
        
def printLLVMError():
    sys.stdout.write("Syntax errors, could not compile")
    
def printLLVM(node):
    if type(node) == list:
        for thing in node:
            if thing:
                if hasattr(thing, 'value'):
                    #print(thing.value)
                    s = getLLVMGlobal(thing)
                    if s and s != "":
                        sys.stdout.write(s)
                else:
                    continue
                    #sys.stdout.write("thing has no value: " + thing)
                if hasattr(thing, "children"):
                    printLLVM(thing.children)
                if hasattr(thing, "next"):
                    printLLVM([thing.next])
    elif type(node) == str:
        sys.stdout.write("in string" + str)
    
def getLLVMGlobal(node):
    if "fn-def" in node.value:
        print("define @" + getLLVMFnDefID(node) + "(" + getLLVMFnParams(node) + "){")
        s = None
        for item in node.children:
            if "block" in item.value:
                s = item
                break
        f = getLLVMLocalFn(s)
        if f:
            print(f)
        print("}")
    elif "struct-def" in node.value:
        pass
    elif "enum-def" in node.value:
        pass

def getLLVMFnDefID(node):
    return node.children[0].children[0].value

registerNum = 0

def getReg():
    registerNum = registerNum + 1
    return "%r" + str(registerNum)

def getLLVMLocalFn(node):
    toPrint = None
    if not node:
        return
    for item in node.children:
        if "lit-dec" in item.value:
            toPrint = getReg() + item.children[0].children.value
        
    if toPrint:
        return toPrint
         
def getLLVMFnParams(node):
    theType = None
    theID = None
    for item in node.children:
        if "fn-params" in item.value:
            for thing in item.children:
                if "fn-param" in thing.value:
                    for stuff in thing.children:
                        if isinstance(stuff, AST):
                        #if type(stuff) is AST:
                            theID = stuff.children[0].value
                        elif isinstance(stuff, str):
                        #elif type(stuff) is str:
                            theType = stuff
    if theType and theID:
        return theType + " " + "%" + theID
    else:
        return ""
                            
            
def checkForMain(functions):
    for f in functions:
        if f.value == 'main':
            l = len(f.children)
            if l == 0:
                return True
            else:
                #outputFile.write("Error: main function has the wrong type.\n")
                sys.exit(-1)
    #outputFile.write("Error: main function not found.\n")
    sys.exit(-1)

# if len(sys.argv) != 3:
#     print("Must provide input and output files")
#     sys.exit(-1)
#   
# outputFile = open(str(sys.argv[2]), 'w')
#   
# text_file = open(sys.argv[1], "r")
# data = text_file.read()
# text_file.close

# data = '''
# fn main() -> () {
#     1 + false;
# }
# '''

file = open("out.ll", 'w')

data = '''
fn main() {
    1 + 2 * 3;
}
'''

# Build the parser
parser = yacc.yacc() 
result = parser.parse(data)

# for token in listOfFunctions:
#     print(token.value)
#      
# for token in listOfStructs:
#     print(token.value)
#      
# for token in listOfEnums:
#     print(token.value)
    
#checkForMain(listOfFunctions)

sys.exit(1)