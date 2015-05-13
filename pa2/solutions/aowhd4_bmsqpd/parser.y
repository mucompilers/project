%{
#include <stdio.h>
#include <glib.h>
#include "ast.h"

void yyerror(char const* msg);
int yylex(void);
int yylineno;

%}

%code requires {
#include <glib.h>
}

%union {
      int num;
      int bool;
      char* str;
      char c;
      GNode* node;
}

%token ABSTRACT
 ALIGNOF
 AS
 BE
 BOX
 BREAK
 CONST
 CONTINUE
 CRATE
 DO
 ELSE
 ENUM
 EXTERN
 FINAL
 FN
 FOR
 IF
 IMPL
 IN
 LET
 LOOP
 MACRO
 MACRO_RULES
 MATCH
 MOD
 MOVE
 MUT
 OFFSETOF
 OVERRIDE
 PRIV
 PUB
 PURE
 REF
 RETURN
 SIZEOF
 STATIC
 SELF
 STRUCT
 SUPER
 TRAIT
 TYPE
 TYPEOF
 UNSAFE
 UNSIZED
 USE
 VIRTUAL
 WHERE
 WHILE
 YIELD
 DBLCOL
 SNGLARROW 
 DBLARROW 
 POUND    
 BASH 
 DOLLAR    
 LBRACK    
 RBRACK    
 LPAREN    
 RPAREN    
 LBRACE    
 RBRACE     
 COMMA    
 SEMI    
 DASH    
 ASTER   
 EXCLAM    
 PLUS    
 SLASH    
 PERCENT    
 AMP    
 BAR    
 CARAT    
 DBLLEFT 
 DBLRIGHT 
 DBLBAR 
 DBLAMP 
 DBLEQ 
 NOTEQ 
 LTHAN    
 GTHAN    
 LEQTHAN 
 GEQTHAN 
 PLUSEQ 
 MINEQ 
 ASTEQ 
 SLASHEQ 
 PERCEQ 
 AMPEQ 
 BAREQ 
 CARATEQ 
 DBLLEFTEQ 
 DBLRIGHTEQ 
 ELIPSE 
 DBLPER 
 EQUAL    
 PERIOD 
 BOOL  
 U8      
 U16    
 U32    
 U64    
 I8      
 I16    
 I32    
 I64    
 F32    
 F64 
 USIZE 
 ISIZE 
 CHAR 
 STR  
 UNDERSCORE
 COLON  
 END 0 "end of file"  
 NEW

%token<num> LITDEC
%token<bool> LITBOOL
%token<c> LITCHAR 
%token<str> LITSTR 
%token<str> ID 
%token UNKNOWN

%left PLUS DASH
%left ASTER SLASH PERCENT
%left EXCLAM
%left UMINUS
%left DEREFERENCE
%left MUTABLEADDR
%right ASSIGNMENT

%left COMMA
%left DBLBAR DBLAMP
%left DBLEQ NOTEQ LTHAN GTHAN LEQTHAN GEQTHAN


%type<node> item fn-def fn-params fn-param pat-type return stmt exp pats pat primitive-lit name-binding pat-deref pat-array pat-enum
           pat-struct id-pat-list id-pat type type-list enum-def enum-ctor-def struct-def type-annotations type-annotation program block crate items literal lookup function compound-lit arm-list stmts loop enum-ctor-params enum-ctor-defs arm-list-item let-stmt pat-list exps assign-ops


%define parse.error verbose 

%%

															/**********************    PROGRAM    *******************/
program: crate 	
											{ 
												ast_print($1);
												$$ = $1;
												return 0; 
											}

															/**********************    CRATE   **********************/ 
crate: items 
											{ 
												GNode * node = ast_node_new(CRATE, NULL);
												g_node_append(node, $1);
												$$ = node;
											}

															/**********************    ITEMS   **********************/
items: item items 
											{ 
												g_node_prepend($2, $1);
												$$ = $2;
											}
   | item
   										{ 
   											GNode * node = ast_node_new(ITEMS, NULL);
   											g_node_append(node, $1);
   											$$ = node;
   										} 

															/**********************    ITEM   **********************/
item: fn-def 
											{ 
												$$ = $1;
											}
   | enum-def 				
   										{ 
   											$$ = $1;
   										}
   | struct-def 																										
   										{
   											$$ = $1; 
   										}

															/**********************    FN-DEF   **********************/
fn-def: FN ID LPAREN RPAREN block 	
											{
												GNode * node = ast_node_new(FN_DEF, NULL);
                                    g_node_prepend(node, $5);
												g_node_prepend(node, ast_node_new(ID, $2));
												$$ = node;
											}
   | FN ID LPAREN RPAREN SNGLARROW return block	
   										{
   											GNode * node = ast_node_new(FN_DEF, NULL);
   											g_node_append(node, ast_node_new(ID, $2));
   											g_node_append(node, $6);
   											g_node_append(node, $7);
   											$$ = node;
   										}
   | FN ID LPAREN fn-params RPAREN block	
   										{
   											GNode * node = ast_node_new(FN_DEF, NULL);
   											g_node_append(node, ast_node_new(ID, $2));
   											g_node_append(node, $4);
   											g_node_append(node, $6);
   											$$ = node;
   										}
   | FN ID LPAREN fn-params RPAREN SNGLARROW return block			
   										{	
   											GNode * node = ast_node_new(FN_DEF, NULL);
   											g_node_append(node, ast_node_new(ID, $2));
   											g_node_append(node, $4);
   											g_node_append(node, $7);
   											g_node_append(node, $8);
   											$$ = node;
   										}

															/**********************    FN-PARAMS   **********************/
fn-params: fn-param COMMA fn-params 
											{
												g_node_prepend($3, $1);
												$$ = $3;
											}
   | fn-param 		
   										{
   											GNode * node = ast_node_new(FN_DEF_ARGS, NULL);
   											g_node_append(node, $1);
   											$$ = node;
   										}

															/**********************    FN-PARAM   **********************/
fn-param: pat-type 
											{
												$$ = $1;
											}

											/**********************    RETURN   **********************/
return: type 																																					
											{
												$$ = $1;
											} 
   | EXCLAM																																						
   										{
   										}

															/**********************    ENUM-DEF   **********************/
enum-def: ENUM ID LBRACE enum-ctor-defs RBRACE
											{
												GNode * node = ast_node_new(ENUM_DEF, NULL);
												g_node_append(node, ast_node_new(ID, $2));
												g_node_append(node, $4);
												$$ = node;
											}

															/**********************    ENUM-CTOR-DEFS   **********************/
enum-ctor-defs: enum-ctor-def COMMA enum-ctor-defs																		
											{
												g_node_prepend($3, $1);
												$$ = $3;
											}
	| enum-ctor-def 																																		
											{
												GNode * node = ast_node_new(ENUM_CTOR_DEFS, NULL);
												g_node_append(node, $1);
												$$ = node;
											}

															/**********************    ENUM-CTOR-DEF   **********************/
enum-ctor-def: ID LPAREN enum-ctor-params RPAREN 																						
											{
												GNode * node = ast_node_new(ENUM_CTOR_DEF, NULL);
												g_node_append(node, ast_node_new(ID, $1));
												g_node_append(node, $3);
												$$ = node;
											}
	| ID
											{
												GNode * node = ast_node_new(ENUM_CTOR_DEF, NULL);
												g_node_append(node, ast_node_new(ID, $1));
												$$ = node;
											}

															/**********************    ENUM-CTOR-PARAMS   **********************/
enum-ctor-params: type COMMA enum-ctor-params
											{
												g_node_prepend($3, $1);
												$$ = $3;
											}
	| type
											{
												GNode * node = ast_node_new(ENUM_CTOR_PARAMS, NULL);
												g_node_append(node, $1);
												$$ = node;
											}
	
															/**********************    PAT-TYPES   **********************/
pat-type: pat COLON type 
											{
												GNode * node = ast_node_new(FN_DEF_ARG, NULL);
												g_node_append(node, $1);
												g_node_append(node, $3);
												$$ = node;
											}

															/**********************    PAT   **********************/
pat: primitive-lit																																		
											{
												GNode * node = ast_node_new(PRIM_LIT_PAT, $1);
												$$ = node;
											}
   | name-binding																																			
   										{
   											$$ = $1;
   										}
   | pat-deref																																				
   										{
   											$$ = $1;
   										}
   | pat-array																																				
   										{
   											$$ = $1;
   										}
   | pat-enum																																					
   										{
   											$$ = $1;
   										}
   | pat-struct																																				
   										{
   											$$ = $1;
   										}
   | UNDERSCORE																																				
   										{
   											GNode * node = ast_node_new(UNDERSCORE, NULL);
   											$$ = node;
   										}
   | exp 																																							
   										{
   											$$ = $1;
   										}

															/**********************    PRIMITIVE-LIT   **********************/
primitive-lit: LITDEC																																	
											{
												GNode * node = ast_node_new(LITDEC, &($1));
												$$ = node;
											}
/*   | TRUE																																							
   										{
   											GNode * node = ast_node_new(T, &($1));
   											$$ = node;
   										}
   | FALSE
   										{
   											GNode * node = ast_node_new(FALS, &($1));
   											$$ = node;
   										}*/
   | LITCHAR																																					
   										{
   											GNode * node = ast_node_new(LITCHAR, &($1));
   											$$ = node;
   										}
   | LITSTR 																																					
   										{
   											GNode * node = ast_node_new(LITSTR, &($1));
   											$$ = node;
   										}
/*   | unit																																							
   										{
   											$$ = $1;
   										}*/

															/**********************    NAME-BINDING   **********************/
name-binding: ID 																																			
											{
												GNode * node = ast_node_new(ID_PAT, NULL);
												g_node_append(node, ast_node_new(ID, $1));
												$$ = node;
											}
   | MUT ID 																																					
   										{
   											GNode * node = ast_node_new(MUT_PAT, NULL);
   											g_node_append(node, ast_node_new(ID, $2));
   											$$ = node;
   										}
   | REF ID 																																					
   										{
   											GNode * node = ast_node_new(REF_PAT, NULL);
   											g_node_append(node, ast_node_new(ID, $2));
   											$$ = node;
   										}
   | REF MUT ID 																																			
   										{
   											GNode * node = ast_node_new(REF_MUT_PAT, NULL);
   											g_node_append(node, ast_node_new(ID, $3));
   											$$ = node;
   										}

															/**********************    EXP   **********************/
exp: literal 																																					{ $$ = $1;}
	| ID 																																								{ $$ = ast_node_new(ID, $1); }
	| LPAREN exp RPAREN 																																{ $$ = $2; }
   | exp PLUS exp 																																		{ GNode * node = ast_node_new(PLUS_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp DASH exp 																																		{ GNode * node = ast_node_new(MINUS_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp ASTER exp 																																		{ GNode * node = ast_node_new(MULT_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp SLASH exp 																																		{ GNode * node = ast_node_new(DIV_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}																										
   | exp PERCENT exp 																																	{ GNode * node = ast_node_new(MOD_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | DASH exp %prec UMINUS																														{ GNode * node = ast_node_new(NEG_E, NULL);
   																																											g_node_append(node, $2);
   																																											$$ = node;
   																																										}
   | exp DBLAMP exp 																																	{ GNode * node = ast_node_new(AND_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp DBLBAR exp 																																	{ GNode * node = ast_node_new(OR_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | EXCLAM exp 																																			{ GNode * node = ast_node_new(NOT_E, NULL);
   																																											g_node_append(node, $2);
   																																											$$ = node;
   																																										}
   | exp LTHAN exp 																																		{ GNode * node = ast_node_new(LTHAN_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp GTHAN exp 																																		{ GNode * node = ast_node_new(GTHAN_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ =  node;
   																																										}
   | exp LEQTHAN exp 																																	{ GNode * node = ast_node_new(LEQTHAN_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp GEQTHAN exp 																																	{ GNode * node = ast_node_new(GEQTHAN_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp DBLEQ exp 																																		{ GNode * node = ast_node_new(EQUAL_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | exp NOTEQ exp 																																		{ GNode * node = ast_node_new(NOTEQ_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
   | AMP exp 																																					{ GNode * node = ast_node_new(AMP_E, NULL);
   																																											g_node_append(node, $2);
   																																											$$ = node;
   																																										}
   | ASTER exp %prec DEREFERENCE																											{ GNode * node = ast_node_new(DEREF_E, NULL);
   																																											g_node_append(node, $2);
   																																											$$ = node;
   																																										}
   | AMP MUT exp %prec MUTABLEADDR																										{ GNode * node = ast_node_new(AMP_MUT_E, NULL);
   																																											g_node_append(node, $3);
   																																											$$ = node;
   																																										}
   | lookup																																						{ $$ = $1;}
   | exp LBRACK exp RBRACK																														{ GNode * node = ast_node_new(ARRAY_IND_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}					
   | WHILE LPAREN exp RPAREN block 																										{ GNode * node = ast_node_new(WHILE_E, NULL);
   																																											g_node_append(node, $5);
   																																											g_node_append(node, $3);
   																																											$$ = node;
   																																										}
   | loop { 
   	$$ = $1;
   }
   | IF LPAREN exp RPAREN block ELSE block 																						{ GNode * node = ast_node_new(IF_E, NULL);
   																																											g_node_append(node, $7);
   																																											g_node_append(node, $5);
   																																											g_node_append(node, $3);
   																																											$$ = node;
   																																										}
   | IF LPAREN exp RPAREN block 																											{ GNode * node = ast_node_new(IF_E, NULL);
   																																											g_node_append(node, $5);
   																																											g_node_append(node, $3);
   																																											$$ = node;
   																																										}
   | MATCH LPAREN exp RPAREN LBRACE arm-list RBRACE 																	{ GNode * node = ast_node_new(MATCH_E, NULL);
   																																											g_node_append(node, $6);
   																																											g_node_append(node, $3);
   																																											$$ = node;
   																																										}
   | function 																																				{ $$ = $1; }
   | BOX DBLCOL NEW LPAREN exp RPAREN																									{ GNode * node = ast_node_new(NEW_BOX_E, NULL);
   																																											g_node_append(node, $5);
   																																											$$ = node;
   																																										}
   | pat-struct
   																																				{ $$ = $1; }
/**********************    ASSIGN-OPS   **********************/
assign-ops: PLUSEQ																																		{ $$ = ast_node_new(PLUSEQ, NULL); }
	| MINEQ																																							{ $$ = ast_node_new(MINEQ, NULL); }
	| ASTEQ																																							{ $$ = ast_node_new(ASTEQ, NULL);}
	| SLASHEQ																																						{ $$ = ast_node_new(SLASHEQ, NULL);}
	| PERCEQ																																						{ $$ = ast_node_new(PERCEQ, NULL);}
	| EQUAL 																																						{ $$ = ast_node_new(EQUAL, NULL);}

/**********************    EXPS   **********************/
exps: exp COMMA exps																																	{ g_node_append($3, $1);
   																																											$$ = $3;
   																																										}
	| exp 																																							{ GNode * node = ast_node_new(FN_ARGS_E, NULL);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}

/**********************    LOOKUP   **********************/
lookup: lookup PERIOD ID 																															{ GNode * node = ast_node_new(LOOKUP_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}
	| exp PERIOD ID 																																		{ GNode * node = ast_node_new(LOOKUP_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}

/**********************    FUNCTION   **********************/
function: ID LPAREN exps RPAREN																												{ GNode * node = ast_node_new(FN_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, ast_node_new(ID, $1));
   																																											$$ = node;
   																																										}
	| ID unit 																																					{ GNode * node = ast_node_new(FN_ARG_E, NULL);
																																												GNode * ast2 = ast_node_new(ID, $1);
   																																											g_node_append(node, ast_node_new(FN_ARG_E, NULL));
   																																											g_node_append(node, ast2);
   																																											$$ = node;
   																																										}

/**********************    ARM-LIST   **********************/   	
arm-list: arm-list-item COMMA arm-list 																								{ g_node_append($3, $1);
   																																											$$ = $3;
   																																										}
	| arm-list-item																																			{ GNode * node = ast_node_new(ARMS_E, NULL);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}

/**********************    ARM-LIST-ITEM   **********************/
arm-list-item: pat-list DBLARROW block																								{ GNode * node = ast_node_new(ARM_E, NULL);
   																																											g_node_append(node, $3);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}

/**********************    PAT-LIST   **********************/
pat-list: pat BAR pat-list 																														{ g_node_append($3, $1); 
																																												$$ = $3;
																																											}
	| pat 																																							{ GNode * node = ast_node_new(MATCH_PAT_E, NULL);
   																																											g_node_append(node, $1);
   																																											$$ = node;
   																																										}

/**********************    PAT-DEREF   **********************/
pat-deref: AMP pat 																																		{ GNode * node = ast_node_new(DEREF_PAT, NULL);
   																																											g_node_append(node, $2);
   																																											$$ = node;
   																																										}

/**********************    PAT-ARRAY   **********************/
pat-array: LBRACK pats RBRACK																													{ GNode * node = ast_node_new(ARRAY_PAT, NULL);
   																																											g_node_append(node, $2);
   																																											$$ = node;
   																																										}
/**********************    LOOP   **********************/
loop: LOOP block {
     GNode * node = ast_node_new(LOOP_BLOCK, NULL);
     g_node_append(node, $2);
     $$ = node;
}

/**********************    PAT-ENUM   **********************/
pat-enum: ID DBLCOL ID LPAREN pats RPAREN 																						{
																																												GNode * node = ast_node_new(ENUM_PAT, NULL);
																																												GNode * ast2 = ast_node_new(ENUM_LIT, NULL);
																																												g_node_append(node, $5);
																																												g_node_append(node, ast2);
																																												g_node_append(ast2, ast_node_new(ID, $3));
																																												g_node_append(ast2, ast_node_new(ID, $1));
																																												$$ = node;
																																											}
   | ID DBLCOL ID 																																		{
																																												GNode * node = ast_node_new(ENUM_PAT, NULL);
																																												GNode * ast2 = ast_node_new(ENUM_LIT, NULL);
																																												g_node_append(node, ast2);
																																												g_node_append(ast2, ast_node_new(ID, $3));
																																												g_node_append(ast2, ast_node_new(ID, $1));
																																												$$ = node;
																																											}


															/**********************    PATS   **********************/
pats: pat COMMA pats 																																	
											{
												g_node_prepend($3, $1);
												$$ = $3;
											}
   | pat 												
   										{
   											GNode * node = ast_node_new(PATS, NULL);
   											g_node_append(node, $1);
   											$$ = node;
   										}


/**********************    PAT-STRUCT   **********************/
pat-struct: ID LBRACE id-pat-list RBRACE 																							{ GNode * node = ast_node_new(STRUCT_PAT, NULL);
																																												g_node_append(node, $3);
																																												g_node_append(node, ast_node_new(ID, $1));
																																												$$ = node;
																																											}
																																													

id-pat-list: id-pat COMMA id-pat-list 																								{ g_node_append($3, $1);
																																												$$ = $3;
																																											}
	| id-pat 																																						{ GNode * node = ast_node_new(STRUCT_ARGS_PAT, NULL);
																																												g_node_append(node, $1);
																																												$$ = node;
																																											}

/**********************    ID-PAT-LIST   **********************/
id-pat: ID COLON pat 																																	{ GNode * node = ast_node_new(STRUCT_ARG_PAT, NULL);
																																												g_node_append(node, $3);
																																												g_node_append(node, ast_node_new(ID, $1));
																																												$$ = node;	
																																											}
/**********************    TYPE   **********************/   
type: U8 																																							{ $$ = ast_node_new(U8, NULL); }
	| I32 																																							{ $$ = ast_node_new(I32, NULL); }
//	| unit 																																							{ $$ = ast_node_new(U8, NULL); }
	| AMP type 																																					{ GNode * node = ast_node_new(REF_T, NULL);
																																												g_node_append(node, $2);
																																												$$ = node;	
																																											}
	| AMP MUT type 																																			{ GNode * node = ast_node_new(MUT_REF_T, NULL);
																																												g_node_append(node, $3);
																																												$$ = node;	
																																											}
	| ID 																																								{ $$ = ast_node_new(ID, NULL); }
	| LBRACK type RBRACK 																																{ GNode * node = ast_node_new(ARRAY_T, NULL);
																																												g_node_append(node, $2);
																																												$$ = node;	
																																											}
	| LBRACK type SEMI LITDEC RBRACK																										{ GNode * node = ast_node_new(ARRAY_T, NULL);
																																												g_node_append(node, ast_node_new(LITDEC, &($4)));
																																												g_node_append(node, $2);
																																												$$ = node;	
																																											}
	| BOX LTHAN type GTHAN 																															{ GNode * node = ast_node_new(BOX_T, NULL);
																																												g_node_append(node, $3);
																																												$$ = node;	
																																											}
	
type-list : type COMMA type-list 																											{
																																												g_node_prepend($3, $1);
																																												$$ = $3;			
																																											}
	| type 																																							{
																																												GNode * node = ast_node_new(ENUM_CTOR_PARAMS, NULL);
																																												g_node_prepend(node, $1);
																																												$$ = node;
																																											}
	
enum-def: ENUM ID LBRACE enum-ctor-defs RBRACE																				{
																																												GNode * node = ast_node_new(ENUM_DEF, NULL);
																																												g_node_prepend(node, $4);
																																												g_node_prepend(node, ast_node_new(ID, $2));
																																												$$ = node;
																																											}

enum-ctor-defs: enum-ctor-def COMMA enum-ctor-defs																		{
																																												g_node_prepend($3, $1);
																																												$$ = $3;		
																																											}
	| enum-ctor-def 																																		{
																																												GNode * node = ast_node_new(ENUM_CTOR_DEFS, NULL);
																																												g_node_prepend(node, $1);
																																												$$ = node;		
																																											}

enum-ctor-def: ID LPAREN type-list RPAREN 																						{
																																												GNode * node = ast_node_new(ENUM_CTOR_DEF, NULL);
																																												g_node_prepend(node, $3);
																																												g_node_prepend(node, ast_node_new(ID, $1));
																																												$$ = node;
																																											}
	| ID 																																								{
																																												GNode * node = ast_node_new(ENUM_CTOR_DEF, NULL);
																																												g_node_prepend(node, ast_node_new(ID, $1));
																																												$$ = node;
																																											}
	
struct-def: STRUCT ID LBRACE type-annotations RBRACE																	{
																																												GNode * node = ast_node_new(STRUCT_DEF, NULL);
																																												g_node_prepend(node, $4);
																																												g_node_prepend(node, ast_node_new(ID, $2));
																																												$$ = node;			
																																											}

type-annotations: type-annotation COMMA type-annotations															{
																																												g_node_prepend($3, $1);
																																												$$ = $3;			
																																											}
	| type-annotation 																																	{
																																												GNode * node = ast_node_new(STRUCT_ARGS, NULL);
																																												g_node_prepend(node, $1);
																																												$$ = node;
																																											}

type-annotation: ID COLON type 																												{
																																												GNode * node = ast_node_new(STRUCT_ARG, NULL);
																																												g_node_prepend(node, $3);
																																												g_node_prepend(node, ast_node_new(ID, $1));
																																												$$ = node;			
																																											}
	
unit: LPAREN RPAREN																																		{}

block: LBRACE stmts RBRACE																														{ $$ = $2; }

stmts: stmt stmts																																			{
																																												g_node_prepend($2, $1);
																																												$$ = $2;
																																											}
	|	stmt																																							{
																																												GNode * node = ast_node_new(BLOCK, NULL);
																																												g_node_prepend(node, $1);				
																																												$$ = node;	
																																											}
	| exp 																																							{
																																												GNode * node = ast_node_new(BLOCK, NULL);
																																												g_node_prepend(node, $1);
																																												$$ = node;
																																											}
	
stmt: exp SEMI																																				{ $$ = $1; }
	| RETURN exp SEMI																																		{
																																												GNode * node = ast_node_new(RET_EXP_STMT, NULL);
																																												g_node_prepend(node, $2);
																																												$$ = node;				 
																																											}
	| RETURN SEMI																																				{
																																												GNode * node = ast_node_new(RET_STMT, NULL);
																																												$$ = node;			
																																											}
	| let-stmt SEMI																																			{ $$ = $1; }
   | loop                                                                                                                  { $$ = $1; }

let-stmt: LET pat 																																		{
																																												GNode * node = ast_node_new(LET_STMT, NULL);
																																												g_node_prepend(node, $2);
																																												$$ = node;
																																											}
	| LET pat COLON type EQUAL exp 																											{
																																												GNode * node = ast_node_new(LET_STMT, NULL);
																																												g_node_prepend(node, $6);
																																												g_node_prepend(node, $4);
																																												g_node_prepend(node, $2);
																																												$$ = node;			
																																											}
	| LET pat EQUAL exp 																															  {
																																												GNode * node = ast_node_new(LET_STMT, NULL);
																																												g_node_prepend(node, $4);
																																												g_node_prepend(node, $2);
																																												$$ = node;	
																																											}
	| LET pat COLON type 																																{
																																												GNode * node = ast_node_new(LET_STMT, NULL);
																																												g_node_prepend(node, $4);
																																												g_node_prepend(node, $2);
																																												$$ = node;	
																																											} 
	
literal: primitive-lit																																{ $$ = $1;}
	| compound-lit																																			{ $$ = $1; }
	
compound-lit: pat-enum 																																{ $$ = $1;}
	| pat-struct																																				{ $$ = $1;}
	| pat-array																																					{ $$ = $1;}

%%


void yyerror(char const* msg) {
      fprintf(stderr, "Error near line %d: %s\n", yylineno, msg);
}
