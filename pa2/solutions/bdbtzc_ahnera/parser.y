%{
#include <stdio.h>
#include <glib.h>
#include "ast.h"

void yyerror(char const* msg);
int yylex(void);
char *yytext;

int yylineno;

%}

%code requires {
#include <glib.h>
}



%union {
      char *value;
      struct ast* ast;
      GList* list;
}

%start ast

%token<value> ID
%token FN
%token LIT_STR
%token LIT_DEC
%token LIT_CHAR
%token STRUCT
%token FN_RETURN_TYPE
%token ENUM
%token TYPE_I32
%token TYPE_U8
%token TYPE_BOOL
%token BOX_NEW
%token RETURN
%token MUT
%token REF
%token NOT
%token TYPE
%token BOX
%token WHILE
%token LOOP
%token IF
%token OR
%token ELSE
%token AND
%token MATCH
%token DEREF
%token ASSIGN
%token ASSIGN_SUB
%token ASSIGN_ADD
%token ASSIGN_MUL
%token ASSIGN_DIV
%token ASSIGN_REM
%token LET
%token NEQ
%token LEQ
%token GEQ
%token GT
%token LT
%token ADD
%token SUB
%token MUL
%token DIV
%token SUB
%token REM
%token EQ
%token OP_OR
%token BOOL_TRUE
%token BOOL_FALSE
%token ADDR_OF
%token WILD
%token BYTE_STR
%token BYTE_CHAR
%token ASSIGN_PAT
%token UNKNOWN

%type<ast> ast ast-list head global-defs items item fn-def fn-params fn-param block stmts stmt exp fn-return-type
%type<ast> types type type-prim type-arr type-ref type-ref-mut type-box type-unit
%type<ast> let pat-id return bin-exp lit struct-def pats pat-struct pat-ids fn-args fn-call fn-arg struct-lits struct-lit compound-lit
%type<ast> field-lookup enum-def enum-ctor-defs enum-ctor-def bin-op array-index pat-array prim-lit

%define parse.error verbose

%%

ast: ast-list

ast-list: ast-list head | { $$ = NULL; }

head: global-defs

global-defs: items

ast: items

items: items item | { $$ = NULL; }

item: fn-def | struct-def | enum-def

fn-def: FN ID '(' fn-params ')' fn-return-type block

enum-def: ENUM ID '{' enum-ctor-defs '}'

enum-ctor-defs: enum-ctor-defs ',' enum-ctor-def | enum-ctor-def

enum-ctor-def: ID | ID  '(' types ')'

struct-def: STRUCT ID '{' pats '}'

pats: pats ',' pat | pat

pat: pat-struct | pat-enum | pat-unit | pat-lit | field-lookups | pat-array | pat-wild | ADDR_OF pat

pat-lit: SUB LIT_DEC | prim-lit

pat-unit: pat-id | pat-ref-id | pat-ref-mut-id | pat-mut-id

pat-id: ID | ID ':' type

pat-ref-id: REF ID

pat-ref-mut-id: REF MUT ID

pat-mut-id: MUT ID

pat-deref: ADDR_OF pat

pat-enum: ID ':' ':' ID '(' pat-enum-ctor-params ')' | ID ':' ':' ID | BOX_NEW '(' pat-enum-ctor-params ')'

pat-enum-ctor-params: pat-enum-ctor-params ',' pat | pat

pat-ids: pat-ids ',' pat-id | pat-id

pat-struct: ID '{' pat-struct-fields '}'

pat-array: '[' pats ']'

pat-struct-fields: pat-struct-fields ',' pat-field | pat-field

pat-field: ID ':' pat

pat-wild: WILD

prim-lit: LIT_CHAR | LIT_STR | LIT_DEC | BOOL_TRUE | BOOL_FALSE | '('')' | BYTE_STR | BYTE_CHAR

fn-return-type: FN_RETURN_TYPE type | FN_RETURN_TYPE NOT | { $$ = NULL; }

fn-params: fn-params ',' fn-param | fn-param | { $$ = NULL; }

fn-param: pat-id | pat-struct | exp

field-lookups: field-lookup | exp '.' field-lookups

field-lookup: exp '.' ID

block: '{' stmts '}' | '{' stmts exp '}'

stmts: stmts stmt | { $$ = NULL; }

stmt: let ';' | return ';' | exp ';'

return: RETURN exp | RETURN

let: LET pat | LET pat ':' type EQ exp | LET pat ':' type | LET pat EQ exp

exp: ID | pat-lit | bin-exp | WHILE '(' exp ')' block | LOOP block | fn-call | BOX_NEW '(' exp ')'
      | compound-lit | field-lookups | array-index | if | match | prim-lit

if: IF '(' exp ')' block | IF '(' exp ')' block ELSE block

match: MATCH '(' exp ')' '{' match-arms '}'

match-arms: match-arms ',' match-arm | match-arm

match-pats: match-pats OR pat | pat

match-arm: match-pats ASSIGN_PAT block

fn-call: ID '(' fn-params ')'

bin-exp: SUB exp | '(' exp ')' | exp bin-op exp | NOT exp | MUL exp | ADDR_OF exp | ADDR_OF MUT exp

bin-op: ADD | SUB | MUL | DIV | OR | ASSIGN | LEQ | GEQ | REM | LT | GT | OP_OR | NEQ | AND | EQ
      | ASSIGN_ADD | ASSIGN_MUL | ASSIGN_SUB | ASSIGN_DIV | ASSIGN_REM

compound-lit: pat-struct | enum-ctor | array-lit

array-lit: pat-array

array-index: exp '[' exp ']'

enum-ctor:  pat-enum

types: types ',' type | type

type: type-prim | type-arr | type-ref | type-ref-mut | type-box | type-unit | ADDR_OF type

type-prim: TYPE_I32 | TYPE_U8 | TYPE_BOOL | '(' ')'

type-arr: '[' type ']' | '[' type ';' LIT_DEC ']'

type-ref: ADDR_OF type

type-ref-mut: ADDR_OF MUT type

type-box: BOX LT type GT

type-unit: ID

%%

void yyerror(char const* msg) {
      fprintf(stderr, "Error on line %d: %s\n", yylineno, msg);
}
