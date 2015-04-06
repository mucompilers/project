%{
#include <stdio.h>
#include "ast.h"
#include <glib.h>
void yyerror(char const* msg);
int yylex(void);
extern int yylineno;
extern char *yytext;
%}

%union {
	int boolean;
	char charactor;
	char * string;
	int number;

      	char * id; 
     	struct ast* ast;
	GList * list;
}

%{
#include <glib.h>

%}

// symbol
%token NEW COLON NAMESPACE POINTER SHARP LBRACKET RBRACKET LPARENT RPARENT LBRACE RBRACE COMMA SEMICOLON ATTRIBUTE SINGLEQUOTA EQUALTO DOLLAR

// operator
%token DOT UNDERSCORE MINUS MULTIPLE EXCLAMATORY PLUS DIVISION PERCENT AND_BIT OR_BIT XOR LSHIFT RSHIFT OR AND DOUBLE_EQUAL NOTEQUAL LESS MORE LESSOREQUAL MOREOREQUAL AS EQUAL PLUSEQUAL MINUSEQUAL MULEQUAL DIVEQUAL REMEQUAL ANDEQUAL OREQUAL XOREQUAL LSHIFTEQUAL RSHIFTEQUAL TRIPOINT DOUPOINT

//primitive
%token EMPTY BOOL U8 U16 U32 U64 I8 I16 I32 I64 BINARY32 BINARY64 USIZE ISIZE CHAR STR IS US

// keyword
%token MACRO_RULES OFFSETOF ABSTRACT ALIGNOF BE BOX BREAK CONST CONTINUE CRATE DO ELSE ENUM EXTERN FINAL FN FOR IF IMPL IN LET LOOP MACRO MATCH MOD MOVE MUT OFFSETPF OVERRIDE PRIV PUB PURE REF RETURN SIZEOF STATIC SELF STRUCT SUPER TRAIT TYPE TYPEOF UNSAFE UNSIZED USEVIRTUAL WHERE WHILE YIELD

%token  END UNKNOWN SYMBOL USE VIRTUAL 

%token<boolean> BOOLEAN
%token<id> ID
%token<charactor> LITCHAR	
%token<string> LITSTR
%token<number> LITDEC 

%type<ast> operator_exp
%type<ast> expression
%type<list> expressions
%type<list> block
%type<list> match_arms
%type<ast> match_arm
%type<list> pats
%type<ast> pat
%type<ast> pri_literal
%type<list> operator_exps
%type<ast> com_literal

%type<list> pat_arr_elems
%type<ast> id
%type<ast> pat_arr
%type<list> pat_enum_params
%type<list> field_inits
%type<ast> field_init
%type<ast> exprs
%type<list> pat_fields
%type<ast> pat_field

%type<ast> statement
%type<list> statements
%type<ast> type
%type<ast> primitive_type
%type<ast> type_arr
%type<ast> type_ref
%type<ast> type_mut_ref
%type<ast> type_box
%type<ast> type_arrs
%type<ast> empty




%type<ast> crate
%type<list> items
%type<ast> item
%type<ast> function
%type<ast> crate_enum
%type<ast> crate_struct
%type<list> parameters
%type<ast> parameter
%type<list> constructors
%type<ast> constructor
%type<list> fields
%type<ast> field
%type<list> types




%start crate

//%define parse.error verbose



%left EQUAL PLUSEQUAL MINUSEQUAL MULEQUAL DIVEQUAL REMEQUAL ANDEQUAL OREQUAL XOREQUAL LSHIFTEQUAL RSHIFTEQUAL 
%left OR
%left AND 
%left DOUBLE_EQUAL NOTEQUAL
%left LESS MORE LESSOREQUAL MOREOREQUAL 
%left PLUS MINUS
%left MULTIPLE DIVISION PERCENT
%left UMINUS 
%left LPARENT RPARENT DOT LBRACKET RBRACKET

%%

crate: items {$$=ast_unaop_g(AST_CRATE,$1);ast_print1($$,0);}

items: items item{$$=g_list_append($1,$2);}
     | item {$$=g_list_append(NULL,$1);}

item: function {$$=$1;}
    | crate_enum {$$=$1;}
    | crate_struct {$$=$1;}

function: FN id LPARENT parameters RPARENT POINTER type block {$$=ast_biquop_lglg(AST_FN_DEF1,$2,$4,$7,$8);}
	| FN id LPARENT parameters RPARENT POINTER EXCLAMATORY block {$$=ast_triop_lgg(AST_FN_DEF2,$2,$4,$8);}
	| FN id EMPTY POINTER type block {$$=ast_triop_llg(AST_FN_DEF3,$2,$5,$6);}
	| FN id EMPTY POINTER EXCLAMATORY block {$$=ast_binop_lg(AST_FN_DEF4,$2,$6);}
	| FN id EMPTY block {$$=ast_binop_lg(AST_FN_DEF4,$2,$4);}
	| FN id LPARENT parameters RPARENT  block {$$=ast_triop_lgg(AST_FN_DEF2,$2,$4,$6);}

parameters: parameters COMMA parameter {$$=g_list_append($1,$3);}
	  | parameter {$$=g_list_append(NULL,$1);}

parameter: pat COLON type {$$=ast_binop_new(AST_FN_PARAM,$1,$3);}


crate_enum:ENUM id LBRACE constructors RBRACE {$$=ast_binop_lg(AST_ENUM_DEF,$2,$4);}

constructors: constructors COMMA constructor {$$=g_list_append($1,$3);}
	    | constructor {$$=g_list_append(NULL,$1);}

constructor: id {$$=ast_unaop_new(AST_ENUM_CTOR_DEF1,$1);}
	   | id LPARENT types RPARENT{$$=ast_binop_lg(AST_ENUM_CTOR_DEF2,$1,$3);}

types: types COMMA type{$$=g_list_append($1,$3);}
     | type {$$=g_list_append(NULL,$1);}

crate_struct: STRUCT id LBRACE fields RBRACE{$$=ast_binop_lg(AST_STRUCT_DEF,$2,$4);}
	    
fields: fields COMMA field {$$=g_list_append($1,$3);}
      | field {$$=g_list_append(NULL,$1);}

field: id COLON type {$$=ast_binop_new(AST_FIELD_DEF,$1,$3);}















statement: LET pat SEMICOLON{$$=ast_unaop_new(AST_LET1, $2);}
	 | LET pat COLON  type SEMICOLON{$$=ast_binop_new(AST_LET2,$2,$4);}
	 | LET pat COLON type EQUAL operator_exp SEMICOLON {$$=ast_triop_new(AST_LET3,$2,$4,$6);}
	 | LET pat EQUAL operator_exp SEMICOLON {$$=ast_binop_new(AST_LET4,$2,$4);}
	 | RETURN SEMICOLON {$$=ast_empty_new(AST_RETURN1);}
	 | RETURN operator_exp SEMICOLON {$$=ast_unaop_new(AST_RETURN2,$2);}
	 | operator_exp SEMICOLON {$$=$1;}

type: primitive_type {$$=$1;}
    | type_arr {$$=$1;}
    | type_ref {$$=ast_unaop_new(AST_TYPE_REF,$1);}
    | type_mut_ref {$$=ast_unaop_new(AST_TYPE_MUT_REF,$1);}
    | type_box {$$=ast_unaop_new(AST_TYPE_BOX,$1);}
    | id {$$=$1;}
primitive_type: EMPTY {$$=ast_empty_new(AST_TYPE_UNIT);}
	      | BOOL {$$=ast_empty_new(AST_TYPE_BOOL);}
	      | U8 {$$=ast_empty_new(AST_TYPE_U8);}
	      | I32 {$$=ast_empty_new(AST_TYPE_I32);}
type_arr:LBRACKET type_arrs RBRACKET{$$=$2;}
type_arrs: type SEMICOLON LITDEC {struct ast* temp=ast_lit_dec_new($3); $$=ast_binop_new(AST_TYPE_ARR1,$1,temp);}
	 | type{$$=ast_unaop_new(AST_TYPE_ARR2,$1);}
	 
type_ref: AND_BIT type {$$=$2;}
type_mut_ref: AND_BIT MUT type {$$=$3;}
type_box: BOX LESS type MORE {$$=$3;}









expression: operator_exp SEMICOLON{$$= $1;}





operator_exp: operator_exp PLUS operator_exp {$$=ast_binop_new(AST_ADD,$1, $3);}
	| operator_exp MINUS operator_exp {$$=ast_binop_new(AST_SUB,$1, $3);}
	| operator_exp MULTIPLE operator_exp {$$=ast_binop_new(AST_MUL,$1, $3);}
	| operator_exp DIVISION operator_exp {$$=ast_binop_new(AST_DIV,$1, $3);}
	| operator_exp PERCENT operator_exp{ $$=ast_binop_new(AST_REM,$1,$3);}

	| operator_exp AND operator_exp {$$=ast_binop_new(AST_AND,$1,$3);}
	| operator_exp OR operator_exp {$$=ast_binop_new(AST_OR,$1,$3);}
	| EXCLAMATORY operator_exp %prec UMINUS{$$=ast_unaop_new(AST_NOT,$2);}

	| operator_exp LESS operator_exp {$$=ast_binop_new(AST_LT,$1,$3);}
	| operator_exp MORE operator_exp {$$=ast_binop_new(AST_GT,$1,$3);}
	| operator_exp LESSOREQUAL operator_exp{$$=ast_binop_new(AST_LEQ,$1,$3);}
	| operator_exp MOREOREQUAL operator_exp{$$=ast_binop_new(AST_GEQ,$1,$3);}
	| operator_exp DOUBLE_EQUAL operator_exp{$$=ast_binop_new(AST_EQ,$1,$3);}
	| operator_exp NOTEQUAL operator_exp { $$=ast_binop_new(AST_NEQ,$1,$3);}

	| pri_literal{$$=$1;}
	| com_literal{$$=$1;}


	| id{$$=$1;}	



	| LPARENT operator_exp RPARENT { $$=$2; }

	| MINUS operator_exp %prec UMINUS{$$=ast_unaop_new(AST_NEG,$2);} 
	| AND_BIT operator_exp %prec UMINUS{$$=ast_unaop_new(AST_ADDR_OF,$2);}
	| MULTIPLE operator_exp %prec UMINUS{$$=ast_unaop_new(AST_DEREF,$2);}
	| AND_BIT MUT operator_exp %prec UMINUS{$$=ast_unaop_new(AST_ADDR_OF_MUT,$3);}

	| operator_exp DOT id{$$=ast_binop_new(AST_FIELD_LOOKUP,$1,$3);}

	| operator_exp LBRACKET operator_exp RBRACKET{$$=ast_binop_new(AST_ARR_INDEX,$1,$3);}

	| operator_exp EQUAL operator_exp{$$=ast_binop_new(AST_ASSIGN,$1,$3);} 
	| operator_exp PLUSEQUAL operator_exp{$$=ast_binop_new(AST_ASSIGN_ADD,$1,$3);}
	| operator_exp MINUSEQUAL operator_exp{$$=ast_binop_new(AST_ASSIGN_SUB,$1,$3);}
	| operator_exp MULEQUAL operator_exp{$$=ast_binop_new(AST_ASSIGN_MUL,$1,$3);}
	| operator_exp DIVEQUAL operator_exp{$$=ast_binop_new(AST_ASSIGN_DIV,$1,$3);}
	| operator_exp REMEQUAL operator_exp{$$=ast_binop_new(AST_ASSIGN_REM,$1,$3);}
	| WHILE LPARENT operator_exp RPARENT block {$$=ast_while_new($3,$5);}
	| WHILE operator_exp block{$$=ast_while_new($2,$3);}



	| LOOP block {$$=ast_loop_new($2);}

	| IF LPARENT operator_exp RPARENT block {$$=ast_if_new1($3,$5);}
	| IF LPARENT operator_exp RPARENT block ELSE block {$$=ast_if_new2($3,$5,$7);}
	| IF operator_exp block {$$=ast_if_new1($2,$3);}
	| IF operator_exp block ELSE block {$$=ast_if_new2($2,$3,$5);}

	

	| MATCH LPARENT operator_exp RPARENT LBRACE match_arms RBRACE {$$=ast_binop_lg(AST_MATCH,$3,$6);}
	| MATCH operator_exp LBRACE match_arms RBRACE {$$=ast_binop_lg(AST_MATCH,$2,$4);}


	| id LPARENT exprs  RPARENT{$$=ast_binop_new(AST_FN_CALL,$1,$3);}
	| id empty {$$=ast_unaop_new(AST_FN_CALL1,$1);}
	| BOX NAMESPACE NEW LPARENT exprs RPARENT {$$=ast_unaop_new(AST_BOX_NEW, $5);}
	| empty {$$=$1;}

empty: EMPTY {$$=ast_empty_new(AST_EMPTY);}



expressions: expressions expression {$$=g_list_append($1,$2);}
	   |expressions operator_exp{$$=g_list_append($1,$2);}
	   |{$$=NULL; }
statements: statements statement{ $$=g_list_append($1,$2);}
	  | statement {$$=g_list_append(NULL,$1);}

block: LBRACE statements RBRACE {$$=$2;}
     | LBRACE RBRACE {$$=NULL;}
     | LBRACE statements operator_exp RBRACE {$$=g_list_append($2,$3);}
     | LBRACE operator_exp RBRACE{$$=g_list_append(NULL,$2);}


match_arms: match_arms COMMA match_arm {$$=g_list_append($1,$3);}
	  | match_arm{$$=g_list_append(NULL,$1);}
match_arm: pats EQUALTO block {$$=ast_binop_gg(AST_MATCH_ARM,$1,$3);}
pats: pats OR_BIT pat {$$=g_list_append($1,$3);}
    | pat {$$=g_list_append(NULL,$1);}
pat: pri_literal{$$=ast_unaop_new(AST_PAT_LIT,$1);}
   | MINUS LITDEC  {struct ast* temp=ast_lit_dec_new($2);$$=ast_unaop_new(AST_PAT_LIT,temp);}
   | id {$$=ast_unaop_new(AST_PAT_ID,$1);}
   | REF id{$$=ast_unaop_new(AST_PAT_REF_ID,$2);}
   | REF MUT id{$$=ast_unaop_new(AST_PAT_REF_MUT_ID,$3);}
   | MUT id {$$=ast_unaop_new(AST_PAT_MUT_ID, $2);}
   | AND_BIT id %prec UMINUS{struct ast* temp=ast_unaop_new(AST_PAT_ID,$2);$$=ast_unaop_new(AST_PAT_DEREF, temp);}
   | LBRACKET pat_arr RBRACKET{$$=ast_unaop_new(AST_PAT_ARR,$2);} 
   | id NAMESPACE id{struct ast* temp=ast_binop_new(AST_ENUM_CTOR,$1,$3);$$=ast_unaop_new(AST_PAT_ENUM, temp);}
   | id NAMESPACE id LPARENT pat_enum_params RPARENT{ struct ast* temp=ast_binop_new(AST_ENUM_CTOR,$1,$3); $$=ast_binop_lg(AST_PAT_ENUM_CTOR_PARAMS, temp, $5);}
   | id  LBRACE pat_fields RBRACE{$$ = ast_binop_lg(AST_PAT_STRUCT,$1,$3);}
   | UNDERSCORE {$$=ast_pat_wild_new();}
   | EMPTY {$$=ast_empty_new(AST_PAT_UNIT);}
pat_enum_params: pat_enum_params COMMA pat{$$ = g_list_append($1,$3);}
	       | pat {$$ = g_list_append(NULL,$1);}
pat_arr: pat_arr_elems {$$=ast_unaop_g(AST_PAT_ELEM,$1);}
pat_arr_elems: pat_arr_elems COMMA pat {$$ = g_list_append($1,$3);}
	     | pat{$$ =g_list_append(NULL,$1);}





id: ID {$$=ast_id_new($1);}
pri_literal: BOOLEAN {$$=ast_bool_new($1);}
	   | LITCHAR {$$=ast_lit_char_new($1);}
	   | LITSTR {$$=ast_lit_str_new($1);}
	   | LITDEC {$$=ast_lit_dec_new($1);}
	   | EMPTY {$$=ast_unit_new();}
	   | MINUS pri_literal  %prec UMINUS{$$=ast_unaop_new(AST_NEG,$2);} 
com_literal: id NAMESPACE id {struct ast* ctor=ast_binop_new(AST_ENUM_CTOR,$1,$3);$$=ast_unaop_new(AST_ENUM,ctor); }
	   | id NAMESPACE id LPARENT operator_exps RPARENT{struct ast* temp=ast_binop_new(AST_ENUM_CTOR,$1,$3); $$=ast_binop_lg(AST_ENUM_CTOR_PARAMS,temp, $5);}
	   | id LBRACE field_inits RBRACE{$$=ast_binop_lg(AST_STRUCT, $1, $3);}
	   | LBRACKET exprs RBRACKET {$$=ast_unaop_new(AST_ARR,$2);}

exprs:operator_exps{$$ = ast_unaop_g(AST_EXPRS,$1);}


operator_exps: operator_exps COMMA operator_exp {$$=g_list_append($1,$3);}
	   | operator_exp {$$=g_list_append(NULL,$1);}


field_inits: field_inits COMMA field_init {$$=g_list_append($1,$3);}
	   | field_init {$$=g_list_append(NULL, $1);}
field_init: id COLON operator_exp {$$=ast_binop_new(AST_FIELD_INIT, $1, $3);}




pat_fields: pat_fields COMMA pat_field {$$=g_list_append($1,$3);}
	 | pat_field {$$=g_list_append(NULL, $1);}
pat_field: id COLON pat {$$=ast_binop_new(AST_PAT_FIELD, $1, $3);}




%%

void yyerror(char const* msg) {
      printf("\n[ERROR]%s:'%s' in line %d\n", msg, yytext, yylineno);
}
