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
%type<ast> expressions
%type<ast> block
%type<ast> match_arms
%type<ast> match_arm
%type<ast> pats
%type<ast> pat
%type<ast> pri_literal
%type<list> operator_exps
%type<ast> com_literal

%type<ast> pat_arr_elems
%type<ast> id
%type<ast> pat_arr
%type<ast> pat_enum_params
%type<ast> ast_fields
%type<list> field_inits
%type<ast> field_init
%type<ast> exprs
%type<ast> pat_fields
%type<ast> pat_field

%type<ast> statement
%type<ast> statements
%type<ast> type
%type<ast> primitive_type
%type<ast> type_arr
%type<ast> type_ref
%type<ast> type_mut_ref
%type<ast> type_box
%type<ast> type_arrs
%type<ast> empty




%type<ast> crate
%type<ast> ast_items
%type<list> items
%type<ast> item
%type<ast> function
%type<ast> crate_enum
%type<ast> crate_struct
%type<ast> ast_params
%type<list> parameters
%type<ast> parameter
%type<ast> constructors
%type<ast> constructor
%type<list> fields
%type<ast> field
%type<ast> types




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

crate: ast_items {$$=ast_unary(AST_CRATE,$1);ast_calc_type($$,NULL);ast_print($$);}
ast_items: items {$$=ast_list(AST_ITEMS,$1);}
items: items item{$$=g_list_append($1, $2);}
     | item {$$=g_list_append(NULL,$1);}
item: function {$$=$1;}
    | crate_enum {}
    | crate_struct {$$=$1;}



function: FN id LPARENT ast_params RPARENT POINTER type block {$$=ast_quater(AST_FN_DEF2,$2,$4,$7,$8);}
	| FN id LPARENT ast_params RPARENT POINTER EXCLAMATORY block {}
	| FN id EMPTY POINTER type block {$$=ast_ternary(AST_FN_DEF1,$2,$5,$6);}
	| FN id EMPTY POINTER EXCLAMATORY block {}
	| FN id EMPTY block {struct ast* temp=ast_unary(AST_TYPE_UNIT,NULL);$$=ast_ternary(AST_FN_DEF1,$2,temp,$4);}
	| FN id LPARENT ast_params RPARENT  block {}
ast_params:parameters{$$=ast_list(AST_FN_PARAMS,$1);}
parameters: parameters COMMA parameter {$$=g_list_append($1,$3);}
	  | parameter {$$=g_list_append(NULL,$1);}
parameter: pat COLON type {$$=ast_binary(AST_FN_PARAM,$1,$3);}

crate_enum:ENUM id LBRACE constructors RBRACE {}
constructors: constructors COMMA constructor {}
	    | constructor {}
constructor: id {}
	   | id LPARENT types RPARENT{}
types: types COMMA type{}
     | type {}
                                         
crate_struct: STRUCT id LBRACE ast_fields RBRACE{$$=ast_binary(AST_STRUCT_DEF,$2,$4);}
ast_fields: fields {$$=ast_list(AST_FIELD_DEFS,$1);}
fields: fields COMMA field {$$=g_list_append($1,$3);}
      | field {$$=g_list_append(NULL,$1);}
field: id COLON type {$$=ast_binary(AST_FIELD_DEF,$1,$3);}


                                         
type: primitive_type {}
      | type_arr {$$=$1;}
      | type_ref {}
      | type_mut_ref {}
      | type_box {}
      | id {$$=$1;}
primitive_type: EMPTY {$$=ast_unary(AST_TYPE_UNIT,NULL);}
      | BOOL {$$=ast_unary(AST_TYPE_BOOL,NULL);}
      | U8 {$$=ast_unary(AST_TYPE_U8,NULL);}
      | I32 {$$=ast_unary(AST_TYPE_I32,NULL);}
type_arr:LBRACKET type_arrs RBRACKET{$$=$2;}
type_arrs: type SEMICOLON LITDEC {struct ast* temp=ast_unary(AST_LIT_DEC,$3);$$=ast_binary(AST_TYPE_ARR2,$1,temp);}
      | type{}
type_ref: AND_BIT type {$$=ast_unary(AST_TYPE_REF,$2);}
type_mut_ref: AND_BIT MUT type {}
type_box: BOX LESS type MORE {$$=ast_unary(AST_TYPE_BOX,$3);}
                                         
                                         
                                         
block: LBRACE statements RBRACE {struct ast* temp=ast_unary(AST_UNIT,NULL);g_list_append($2, temp);$$=ast_list(AST_BLOCK,$2);}
      | LBRACE RBRACE {struct ast* temp = ast_unary(AST_UNIT,NULL);GList* list=g_list_append(NULL,temp);$$=ast_list(AST_BLOCK,list);}
      | LBRACE statements operator_exp RBRACE {GList* list=g_list_append($2,$3);$$=ast_list(AST_BLOCK,list);}
      | LBRACE operator_exp RBRACE{GList* list=g_list_append(NULL,$2);$$=ast_list(AST_BLOCK,list);}
                                        


statement: LET pat SEMICOLON{$$=ast_unary(AST_LET1,$2);}
	 | LET pat COLON  type SEMICOLON{$$=ast_binary(AST_LET2,$2,$4);}
	 | LET pat COLON type EQUAL operator_exp SEMICOLON {$$=ast_ternary(AST_LET3,$2,$4,$6);}
	 | LET pat EQUAL operator_exp SEMICOLON {$$=ast_binary(AST_LET4,$2,$4);}
	 | RETURN SEMICOLON {}
	 | RETURN operator_exp SEMICOLON {$$=ast_unary(AST_RETURN,$2);}
	 | operator_exp SEMICOLON {$$=ast_unary(AST_STMT_EXP,$1);}

statements: statements statement{$$=g_list_append($1,$2);}
     | statement {$$=g_list_append(NULL, $1);}
  
                                         
                                         
pats: pats OR_BIT pat {}
      | pat {}
pat: pri_literal{}
      | MINUS LITDEC  {}
      | id {$$=ast_unary(AST_PAT_ID,$1);}
      | REF id{}
      | REF MUT id{}
      | MUT id {$$=ast_unary(AST_PAT_MUT_ID,$2);}
      | AND_BIT id %prec UMINUS{}
      | LBRACKET pat_arr RBRACKET{}
      | id NAMESPACE id{}
      | id NAMESPACE id LPARENT pat_enum_params RPARENT{ }
      | id  LBRACE pat_fields RBRACE{}
      | UNDERSCORE {}
      | EMPTY {}
pat_enum_params: pat_enum_params COMMA pat{}
      | pat {}
pat_arr: pat_arr_elems {}
pat_arr_elems: pat_arr_elems COMMA pat {}
      | pat{}
                                         
                                         

expression: operator_exp SEMICOLON{}
operator_exp: operator_exp PLUS operator_exp {$$=ast_binary(AST_ADD,$1,$3);}
	| operator_exp MINUS operator_exp {$$=ast_binary(AST_SUB,$1,$3);}
	| operator_exp MULTIPLE operator_exp {$$=ast_binary(AST_MUL,$1,$3);}
	| operator_exp DIVISION operator_exp {$$=ast_binary(AST_DIV,$1,$3);}
	| operator_exp PERCENT operator_exp{$$=ast_binary(AST_REM,$1,$3);}

	| operator_exp AND operator_exp {$$=ast_binary(AST_AND,$1,$3);}
	| operator_exp OR operator_exp {$$=ast_binary(AST_OR,$1,$3);}
	| EXCLAMATORY operator_exp %prec UMINUS{$$=ast_unary(AST_NOT,$2);}

	| operator_exp LESS operator_exp {$$=ast_binary(AST_LT,$1,$3);}
	| operator_exp MORE operator_exp {$$=ast_binary(AST_GT,$1,$3);}
	| operator_exp LESSOREQUAL operator_exp{$$=ast_binary(AST_LEQ,$1,$3);}
	| operator_exp MOREOREQUAL operator_exp{$$=ast_binary(AST_GEQ,$1,$3);}
	| operator_exp DOUBLE_EQUAL operator_exp{$$=ast_binary(AST_EQ,$1,$3);}
	| operator_exp NOTEQUAL operator_exp {$$=ast_binary(AST_NEQ,$1,$3);}

	| pri_literal{$$=$1;}
	| com_literal{$$=$1;}

	| id{}

	| LPARENT operator_exp RPARENT {$$=$2;}

	| MINUS operator_exp %prec UMINUS{}
	| AND_BIT operator_exp %prec UMINUS{$$=ast_unary(AST_ADDR_OF,$2);}
	| MULTIPLE operator_exp %prec UMINUS{$$=ast_unary(AST_DEREF,$2);}
	| AND_BIT MUT operator_exp %prec UMINUS{}

	| operator_exp DOT id{$$=ast_binary(AST_FIELD_LOOKUP,$1,$3);}

	| operator_exp LBRACKET operator_exp RBRACKET{$$=ast_binary(AST_ARR_INDEX,$1,$3);}

	| operator_exp EQUAL operator_exp{$$=ast_binary(AST_ASSIGN,$1,$3);}
	| operator_exp PLUSEQUAL operator_exp{$$=ast_binary(AST_ASSIGN_ADD,$1,$3);}
	| operator_exp MINUSEQUAL operator_exp{$$=ast_binary(AST_ASSIGN_SUB,$1,$3);}
	| operator_exp MULEQUAL operator_exp{$$=ast_binary(AST_ASSIGN_MUL,$1,$3);}
	| operator_exp DIVEQUAL operator_exp{$$=ast_binary(AST_ASSIGN_DIV,$1,$3);}
	| operator_exp REMEQUAL operator_exp{$$=ast_binary(AST_ASSIGN_REM,$1,$3);}
	| WHILE LPARENT operator_exp RPARENT block {$$=ast_binary(AST_WHILE,$3,$5);}
	| WHILE operator_exp block{$$=ast_binary(AST_WHILE,$3,5);}

	| LOOP block {$$=ast_unary(AST_LOOP,$2);}

	| IF LPARENT operator_exp RPARENT block {$$=ast_binary(AST_IF1,$3,$5);}
	| IF LPARENT operator_exp RPARENT block ELSE block {$$=ast_ternary(AST_IF2,$3,$5,$7);}
	| IF operator_exp block {$$=ast_binary(AST_IF1,$2,$3);}
	| IF operator_exp block ELSE block {$$=ast_ternary(AST_IF2,$2,$3,$5);}

	| MATCH LPARENT operator_exp RPARENT LBRACE match_arms RBRACE {}
	| MATCH operator_exp LBRACE match_arms RBRACE {}

	| id LPARENT exprs  RPARENT{$$=ast_binary(AST_FN_CALL1,$1,$3);}
	| id EMPTY {$$=ast_unary(AST_FN_CALL2,$1);}
	| id empty {}
	| BOX NAMESPACE NEW LPARENT exprs RPARENT {$$=ast_unary(AST_BOX_NEW,$5);}
	| empty {$$=$1;}

empty: EMPTY {$$=ast_unary(AST_UNIT,NULL);}

expressions: expressions expression {}
	   |expressions operator_exp{}
	   |{ }
                                         
exprs:operator_exps{$$=ast_list(AST_EXPRS,$1);}
                                         
operator_exps: operator_exps COMMA operator_exp {$$=g_list_append($1,$3);}
      | operator_exp {$$=g_list_append(NULL,$1);}

match_arms: match_arms COMMA match_arm {}
	  | match_arm{}
match_arm: pats EQUALTO block {}



id: ID {$$=ast_id($1);}
pri_literal: BOOLEAN {if($1==1)$$=ast_unary(AST_TRUE,NULL);
	   	      else $$=ast_unary(AST_FALSE,NULL);}
	   | LITCHAR {$$=ast_lit_char($1);}
	   | LITSTR {$$=ast_lit_str($1);}
	   | LITDEC {$$=ast_lit_dec($1);}
	   | EMPTY {$$=ast_unary(AST_UNIT,NULL);}
	   | MINUS pri_literal  %prec UMINUS{}
com_literal: id NAMESPACE id { }
	   | id NAMESPACE id LPARENT operator_exps RPARENT{}
	   | id LBRACE ast_fields RBRACE{$$=ast_binary(AST_STRUCT,$1,$3);}
	   | LBRACKET exprs RBRACKET {$$=ast_unary(AST_TYPE_ARR1,$2);}

ast_fields: field_inits{$$=ast_list(AST_FIELD_INITS,$1);}
field_inits: field_inits COMMA field_init {$$=g_list_append($1,$3);}
	   | field_init {$$=g_list_append(NULL,$1);}
field_init: id COLON operator_exp {$$=ast_binary(AST_FIELD_INIT,$1,$3);}

pat_fields: pat_fields COMMA pat_field {}
	 | pat_field {}
pat_field: id COLON pat {}




%%

void yyerror(char const* msg) {
      printf("\n[ERROR]%s:'%s' in line %d\n", msg, yytext, yylineno);
}
