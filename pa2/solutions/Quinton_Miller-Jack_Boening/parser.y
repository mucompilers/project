/*
  CS4430 - Compilers
  Assignment Three
  Quinton Miller
  Jack Boening
*/

%{
#include <stdio.h>
#include "ast.h"

void yyerror(char const* msg);
int yylex(void);

%}

%union {
      char *string;
      struct ast *ast;
      struct list *list;
}

%token<string> LITCHAR LITSTRING LITDEC LITBOOL ID

%token NEW ABSTRACT ALIGNOF AS BE BOX BREAK CONST CONTINUE CRATE DO ELSE ENUM EXTERN FALSE FINAL FN FOR IF IMPL IN LET LOOP MACRO MACRO_RULES MATCH MOD MOVE MUT OFFSETOF OVERRIDE PRIV PUB PURE REF RETURN SIZEOF STATIC SELF STRUCT SUPER TRUE TRAIT TYPE TYPEOF UNSAFE UNSIZED USE VIRTUAL WHERE WHILE YIELD BOOL UEIGHT USIXTEEN UTHREETWO USIXFOUR IEIGHT ISIXTEEN ITHREETWO ISIXFOUR FTHREETWO FSIXFOUR USIZE ISIZE CHAR STR CLASSACCESS OBJECTACCESS PIPE POUND POUNDEXCLAMATION APOSTRAPHE DOLLAR LBRACKET RBRACKET LPAREN RPAREN LSQUIGBRACKET RSQUIGBRACKET COMMA SEMICOLON COLON SUB MUL NOT ADD DIV MODOP AND OR EXCLUSIVEOR LEFTSHIFT RIGHTSHIFT WILDCARD THREEDOTS LOGICALOR LOGICALAND EQUALTO NOTEQUAL LESSTHAN GREATERTHAN LESSTHANOREQUAL GREATERTHANOREQUAL PLUSEQUAL SUBEQUAL TIMESEQUAL DIVEQUAL MODEQUAL ANDEQUAL OREQUAL EXCLUSIVEOREQUAL LEFTSHIFTEQUAL RIGHTSHIFTEQUAL UNKNOWN EQU PERIOD WC

%type<list> stmts items fn_params fn_params_seq exps exps_seq field_defs fn_args body match_arms pat_fields match_pats enum_params pats types
%type<ast> function fn_param id stmt type block item type_ref box loop return exp box_new struct field_def fn_call fn_arg arg_struct param_arg while if match pat_struct pat_field field_lookup lit p_lit match_arm pat let arith comp add sub mul div mod neg lt leq gt geq eq neq or and not addr deref addr_of_mut arith1 arith2 ptr arith3 enum enum_param arith4 assign type_arr arr arr_index unit enum_con pat_enum pat_arr

%define parse.error verbose
%start crate

%left LOGICALOR 
%left LOGICALAND
%left LESSTHAN LESSTHANOREQUAL GREATERTHAN GREATERTHANOREQUAL EQUALTO NOTEQUAL
%right NOT
%left ADD SUB 
%left MUL DIV MODOP


%%
crate: items { print_AST( build_AST("crate", push(NULL, build_AST("items", $1))), 0 ); printf("\n"); };    

items: item items { $$ = push(NULL, $1);
                    $$->next = $2;       }
     | item       { $$ = push(NULL, $1); }
     |            { $$ = NULL;           };

item: function { $$ = $1; }
    | struct   { $$ = $1; }
    | enum     { $$ = $1; };

stmts: stmt SEMICOLON stmts { $$ = push(NULL, $1);
                              $$->next = $3;        }
     | stmt                 { $$ = push(NULL, $1);  }
     |                      { $$ = NULL;            };

stmt: return   { $$ = $1; } 
    | exp      { $$ = $1; }
    | let      { $$ = $1; }
    | function { $$ = $1; };
  
exps_seq: exps { $$ = $1;   }
        |      { $$ = NULL; };

exps: exp COMMA exps { $$ = push(NULL, $1); $$->next = $3; }
    | exp            { $$ = push(NULL, $1);                };
    
exp: LPAREN exp RPAREN { $$ = $2;                          }
   | arith1            { $$ = $1;                          }
   | loop              { $$ = $1;                          }
   | while             { $$ = $1;                          }
   | box_new           { $$ = $1;                          }
   | if                { $$ = $1;                          }
   | match             { $$ = $1;                          }
   | arg_struct        { $$ = $1;                          }
   | assign            { $$ = $1;                          }
   | arr               { $$ = $1;                          }
   | arr_index         { $$ = $1;                          }
   | unit              { $$ = $1;                          }
   |                   { $$ = NULL;                        };

arith1: add    { $$ = $1; }
      | sub    { $$ = $1; }
      | arith2 { $$ = $1; }
      ;
     
arith2: mul           { $$ = $1; }
      | div           { $$ = $1; }
      | mod           { $$ = $1; }
      | neg           { $$ = $1; }
      | arith3        { $$ = $1; };
      
arith3: LPAREN arith1 RPAREN { $$ = $2; }
      | fn_call              { $$ = $1; }
      | comp                 { $$ = $1; }
      | field_lookup         { $$ = $1; }
      | arith4               { $$ = $1; };

arith4: id                   { $$ = $1; }
      | ptr                  { $$ = $1; }
      | lit                  { $$ = $1; };
      
add: arith1 ADD arith1    { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("add", l); }; 
sub: arith1 SUB arith1    { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("sub", l); }; 
mul: arith1 MUL arith1    { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("mul", l); }; 
div: arith1 DIV arith1    { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("div", l); }; 
mod: arith1 MODOP arith1  { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("rem", l); }; 
neg: SUB arith4           { $$ = build_AST("neg", push(NULL, $2));                                  };

comp: lt  { $$ = $1; }
    | leq { $$ = $1; }
    | gt  { $$ = $1; }
    | geq { $$ = $1; }
    | eq  { $$ = $1; }
    | neq { $$ = $1; }
    | or  { $$ = $1; }
    | and { $$ = $1; }
    | not { $$ = $1; };

lt:  arith1 LESSTHAN arith1           { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("lt", l);  };
leq: arith1 LESSTHANOREQUAL arith1    { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("leq", l); };
gt:  arith1 GREATERTHAN arith1        { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("gt", l);  };
geq: arith1 GREATERTHANOREQUAL arith1 { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("geq", l); };
eq:  arith1 EQUALTO arith1            { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("eq", l);  };
neq: arith1 NOTEQUAL arith1           { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("neq", l); };
or:  arith1 LOGICALOR arith1          { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("or", l);  };
and: arith1 LOGICALAND arith1         { List * l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("and", l); };
not: NOT arith1                       { $$ = build_AST("not", push(NULL, $2));                                   };

ptr: addr        { $$ = $1; }
   | deref       { $$ = $1; }
   | addr_of_mut { $$ = $1; };

addr:        AND arith4     { $$ = build_AST("addr-of", push(NULL, $2));     }
    |        AND neg        { $$ = build_AST("addr-of", push(NULL, $2));     };
deref:       MUL arith4     { $$ = build_AST("deref", push(NULL, $2));       }
     |       MUL neg        { $$ = build_AST("deref-of", push(NULL, $2));    };
addr_of_mut: AND MUT arith4 { $$ = build_AST("addr-of-mut", push(NULL, $3)); }
           | AND MUT neg    { $$ = build_AST("addr-of-mut", push(NULL, $3));    };
;

unit: LPAREN RPAREN { $$ = build_AST("unit", NULL); };

assign: exp EQU exp        { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign", l);     }
      | exp PLUSEQUAL exp  { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign-add", l); }
      | exp SUBEQUAL exp   { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign-sub", l); }
      | exp DIVEQUAL exp   { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign-div", l); }
      | exp TIMESEQUAL exp { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign-mul", l); }
      | exp MODEQUAL exp   { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign-rem", l); }
      | exp ANDEQUAL exp   { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign-and", l); }
      | exp OREQUAL exp    { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("assign-or", l);  };

let: LET pat COLON type EQU exp { List *l; l = push(NULL, $2); 
                                  l = push(l, $4); l = push(l, $6); 
                                  $$ = build_AST("let", l);                  }
   | LET pat COLON type         { List *l; l = push(NULL, $2);
                                  l = push(l, $4); $$ = build_AST("let", l); }
   | LET pat EQU exp            { List *l; l = push(NULL, $2); 
                                  l = push(l, $4); 
                                  $$ = build_AST("let", l);                  }
   | LET pat                    { $$ = build_AST("let", push(NULL, $2));     };

field_lookup: exp PERIOD id { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("field-lookup", l); };

while: WHILE LPAREN exp RPAREN block { List *l; l = push(NULL, $3); l = push(l, $5); $$ = build_AST("while", l); };

loop: LOOP block { $$ = build_AST("loop", push(NULL, $2)); };

if: IF LPAREN exp RPAREN block { List *l; l = push(NULL, $3); l = push(l, $5); $$ = build_AST("if", l); }
  | if ELSE block              { $1->children = push($1->children, $3); $$ = $1;                        };

match: MATCH LPAREN exp RPAREN LSQUIGBRACKET match_arms RSQUIGBRACKET {
         List *l; l = push(NULL, $3); l = push(l, build_AST("match-arms", $6)); ;
         $$ = build_AST("match", l);                                  };
         

match_arms: match_arm COMMA match_arms { $$ = push(NULL, $1); $$->next = $3; }
          | match_arm                  { $$ = push(NULL, $1);                };

match_arm: match_pats PIPE block{ List *l; l = push(NULL, build_AST("pats", $1)); 
                                  l = push(l, $3); $$ = build_AST("match-arm", l);     };

match_pats: pat OR match_pats { $$ = push(NULL, $1); $$->next = $3; }
          | pat               { $$ = push(NULL, $1);                };
          
pats: pat COMMA pats { $$ = push(NULL, $1); $$->next = $3; }
          | pat               { $$ = push(NULL, $1);                };
          
pat: p_lit         { $$ = build_AST("pat-lit", push(NULL, $1));        }
   | LPAREN RPAREN { $$ = build_AST("pat-unit", NULL);                 }
   | WC            { $$ = build_AST("pat-wild", NULL);                 }
   | SUB p_lit     { $$ = build_AST("pat-lit", push(NULL, $2));        }
   | id            { $$ = build_AST("pat-id", push(NULL, $1));         }
   | REF id        { $$ = build_AST("pat-ref-id", push(NULL, $2));     }
   | REF MUT id    { $$ = build_AST("pat-ref-mut-id", push(NULL, $3)); }
   | MUT id        { $$ = build_AST("pat-mut-id", push(NULL, $2));     }
   | AND pat       { $$ = build_AST("pat-deref", push(NULL, $2));      }
   | pat_arr       { $$ = $1;                                          }
   | pat_enum      { $$ = $1;                                          }
   | pat_struct    { $$ = $1;                                          };
   
pat_arr: LBRACKET pats RBRACKET { $$ = build_AST("pat-arr", push(NULL, build_AST("pat-arr-elems", $2))); };

pat_struct: id LSQUIGBRACKET pat_fields RSQUIGBRACKET { List *l; l = push(NULL, $1); l = push(l, build_AST("pat-fields", $3));
                                                        $$ = build_AST("pat-struct", l);                                       };

pat_fields: pat_field COMMA pat_fields { $$ = push(NULL, $1); $$->next = $3; }
          | pat_field                  {$$ = push(NULL, $1);                 };

pat_field: id COLON pat { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("pat-field", l); };   
   
arr: LBRACKET exps RBRACKET { $$ = build_AST("arr", push(NULL, build_AST("exprs", $2))); };   
  
arr_index: exp LBRACKET exp RBRACKET { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("arr-index", l); };

function: FN id LPAREN fn_params_seq RPAREN block                   {
            List *l; l = push(NULL, $2);
            if ($4) l = push(l, build_AST("fn-params", $4));
            l = push(l, $6);
            $$ = build_AST("fn-def", l);                        }
        | FN id LPAREN fn_params_seq RPAREN OBJECTACCESS type block {
            List *l; l = push(NULL, $2);
            if ($4) l = push(l, build_AST("fn-params", $4));
            if ($7) l = push(l, $7);
            l = push(l, $8);
            $$ = build_AST("fn-def", l);                        };

fn_call: id LPAREN fn_args RPAREN { List *l; l = push(NULL, $1);
                                    if ($3) l = push(l, build_AST("exprs", $3));
                                    else l = push(l, build_AST("", NULL));
                                    $$ = build_AST("fn-call", l);                };

fn_args: fn_arg fn_args { $$ = push(NULL, $1);
                          $$->next = $2;       }
       |                { $$ = NULL;           };

fn_arg: fn_arg COMMA { $$ = $1; }
      | lit        { $$ = $1;                  }
      | id         { $$ = $1;                  }
      | box_new    { $$ = $1;                  }
      | fn_call    { $$ = $1;                  }
      | arg_struct {$$ = $1;                   };
  
lit: p_lit    { $$ = $1; }
   | enum_con { $$ = $1; };
  
p_lit: LITDEC    { $$ = build_AST("lit-dec", NULL);  }
     | LITCHAR   { $$ = build_AST("lit-char", NULL); }
     | LITBOOL   { $$ = build_AST($1, NULL);         }
     | LITSTRING { $$ = build_AST("lit-str", NULL);  };
   
id: ID {
	$$ = build_AST("id", push(NULL, build_AST($1, NULL))) ; 
};

fn_params_seq: fn_params { $$ = $1;   }
             |           { $$ = NULL; };

fn_params: fn_param COMMA fn_params { $$ = push(NULL, $1); $$->next = $3; }
         | fn_param                 { $$ = push(NULL, $1);                };

fn_param: pat COLON type { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("fn-param", l); }
        | pat COLON id   { List *l; l = push(NULL, $1); l = push(l, $3); $$ = build_AST("fn-param", l); };

types: type COMMA types { $$ = push(NULL, $1); $$->next = $3; }
     | type             { $$ = push(NULL, $1);                }

type: BOOL         { $$ = build_AST("type-bool", NULL);              }
    | UEIGHT       { $$ = build_AST("type-u8", NULL);                }
    | USIXTEEN     { $$ = build_AST("type-u16", NULL);               }
    | UTHREETWO    { $$ = build_AST("type-u32", NULL);               }
    | USIXFOUR     { $$ = build_AST("type-u64", NULL);               }
    | IEIGHT       { $$ = build_AST("type-i8", NULL);                }
    | ISIXTEEN     { $$ = build_AST("type-i16", NULL);               }
    | ITHREETWO    { $$ = build_AST("type-i32", NULL);               }
    | ISIXFOUR     { $$ = build_AST("type-i64", NULL);               }
    | FTHREETWO    { $$ = build_AST("type-f32", NULL);               }
    | FSIXFOUR     { $$ = build_AST("type-f64", NULL);               }
    | CHAR         { $$ = build_AST("type-char", NULL);              }
    | STR          { $$ = build_AST("type-string", NULL);            }
    | box          { $$ = $1;                                        }
    | NOT          { $$ = NULL;                                      }
    | type_ref     { $$ = $1;                                        }
    | id           { $$ = $1;                                        }
    | AND MUT type { $$ = build_AST("type-ref-mut", push(NULL, $3)); }
    | type_arr     { $$ = $1;                                        }
    | LPAREN RPAREN{ $$ = build_AST("type-unit", NULL);              }
    |              { $$ = NULL;                                      };
    
type_arr: LBRACKET type RBRACKET                { $$ = build_AST("type-arr", push(NULL, $2));   }
        | LBRACKET type SEMICOLON p_lit RBRACKET { List *l; l = push(NULL, $2); l = push(l, $4);
                                                  $$ = build_AST("type-arr", l);                };
                                                  
type_ref: AND type {$$ = build_AST("type-ref", push(NULL, $2)); };

box: BOX LESSTHAN type GREATERTHAN { $$ = build_AST("type-box", push(NULL, $3)); };

box_new: BOX CLASSACCESS NEW LPAREN exp RPAREN { $$ = build_AST("box-new", push(NULL, build_AST("exprs", push(NULL, $5)))); };

struct: STRUCT id LSQUIGBRACKET field_defs RSQUIGBRACKET { List *l; l = push(NULL, $2); l = push(l, build_AST("field-defs", $4));
                                                            $$ = build_AST("struct-def", l);                              };
                           
                           
field_defs: field_def field_defs { $$ = push(NULL, $1); $$->next = $2; }
          | field_def            { $$ = push(NULL, $1);                };
          
field_def: field_def COMMA { $$ = $1;                         }
         | id COLON type   { List *l; 
                             l = push(NULL, $1); 
                             l = push(l, $3); 
                             $$ = build_AST("field-def", l);  };

block: LSQUIGBRACKET stmts RSQUIGBRACKET { $$ = build_AST("block", $2); };

return: RETURN     { $$ = build_AST("return", NULL);           }
      | return exp { $$ = build_AST("return", push(NULL, $2)); };

arg_struct: id LSQUIGBRACKET body RSQUIGBRACKET { List* l; l = push(NULL, $1); l = push(l, build_AST("field-inits", $3));
        $$ = build_AST("struct", l);
}

body: param_arg body {$$ = push(NULL, $1); $$->next = $2; }
    | param_arg      {$$ = push(NULL, $1);                }

param_arg: param_arg COMMA {$$ = $1; }
        | id COLON exp {
                List* l;
                        l = push(NULL, $1);
                        l = push(l, $3);
                        $$ = build_AST("field-init", l);
        }
enum_con: enum_con LPAREN exps RPAREN   { $$->children = push($$->children, build_AST("exprs", $3)); }
        | id CLASSACCESS id             { List *l; l = push(NULL, $1); l = push(l, $3);
                                          $$ = build_AST("enum", push(NULL, build_AST("enum-ctor", l)));                            };

pat_enum: pat_enum LPAREN pats RPAREN   { $$->children = push($$->children, build_AST("pat-enum-ctor-params", $3)); }
        | id CLASSACCESS id             { List *l; l = push(NULL, $1); l = push(l, $3);
                                          $$ = build_AST("pat-enum", push(NULL, build_AST("enum-ctor", l)));                            };
                                          
enum: ENUM id LSQUIGBRACKET enum_params RSQUIGBRACKET {
        List* l; l = push(NULL, $2);
        if($4) l = push(l, build_AST("enum-ctor-defs", $4));
        $$ = build_AST("enum-def", l);
};

enum_params: enum_param enum_params {$$ = push(NULL, $1);
        $$->next = $2; };
        | {$$ = NULL;};

enum_param: enum_param COMMA {$$ = $1;}
          | enum_param LPAREN types RPAREN { $$->children = push($$->children, build_AST("enum-ctor-params", $3)); }
          | id {$$ = build_AST("enum-ctor-def", push(NULL, $1));};



%%

void yyerror(char const* msg) {
      fprintf(stderr, "Error: %s\n", msg);
}
