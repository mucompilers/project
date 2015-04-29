%{	//Code used in '.y' file
	#include <stdio.h>
	#include "ast.h"
	#include <glib.h>
	#include "env.h"
	#include "type_annotate.h"

	void yyerror(char const* msg);
	int yylex(void);

%}


%code requires {
#include "ast.h"
#include <glib.h>
}


%union {
	struct {
		int line_number;
		int num;
		char * str;
		char c;
		int booln;
		GNode * gast;
	};
}

%define parse.error verbose

%token 	BOOL_OR BOOL_AND ADD NEGATIVE_SIGN 
 	ASTERISK DIV EXCLAMATION EQ NE REM
	LT GT LE GE ADDRESS LPAREN RPAREN
	MUT

	/*MISC TOKENS*/

	  AS 
          
          SHL SHR 
          BITAND BITXOR BITOR 
          ASSIGNMENT RANGE

          PLUS_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN
          REM_ASSIGN BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
          SHL_ASSIGN SHR_ASSIGN

          PATH FUNCTION_ARROW MATCH_ARROW DIRECTIVE DIRECTIVE_FEATURE APOST
          DOLLAR LSQUARE RSQUARE LCURLY RCURLY TRIPLE_DOT DOT COMMA COLON
          SEMICOLON 

          ABSTRACT ALIGNOF BE BOX
          BREAK CASE CONST CONTINUE CRATE DO
          ELSE ENUM EXTERN /*FALS*/ FINAL
          FN FOR IF IMPL IN
          LET LOOP MACRO MACRO_RULES MATCH MOD
          MOVE NEW OFFSETOF OVERRIDE PRIV
          PUB PURE REF RETURN SIZEOF
          STATIC SELF STRUCT SUPER /*TRU*/
          TRAIT TYPE TYPEOF UNKNOWN UNSAFE UNSIZED
          USE VIRTUAL WHERE WHILE YIELD

          UNIT BOOL U8 U16 U32 U64 I8 I16
          I32 I64 F32 F64 USIZE ISIZE CHAR
          STR UNDERSCORE


%type<gast>  item fn-def fn-params fn-param pat-type-annot ret enum-def enum-ctor-defs
	     enum-ctor-def ctor-types struct-def type-annots type-annot block stmts stmt
	     let exp or and eq neq lt gt leq geq add sub mul div rem neg not deref addr-of
	     addr-of-mut assign fn-call exps if while loop box-new arr-index flup
	     match match-arms match-arm pat-or pat name-bind pat-deref pat-arr pat-enum
	     pat-struct id-pat-annots id-pat-annot type literal prim-lit comp-lit lit-enum
	     enum-exps lit-struct id-exp-annots id-exp-annot lit-array lit-bool array-elmts ctor-elmts
	     program crate items

/*END MISC TOKENS*/
%token<num> LITDEC
%token<str> ID
%token<str> LITSTR//unused
%token<c> LITCHAR	
%token<booln> FALS
%token<booln> TRU


%left COMMA
%right PLUS_ASSIGN SUB_ASSIGN DIV_ASSIGN MUL_ASSIGN REM_ASSIGN ASSIGNMENT/*PRECEDENCE TOKEN*/
%left BOOL_OR 
%left BOOL_AND
%left EQ NE LT GT LE GE
%left ADD NEGATIVE_SIGN
%left ASTERISK DIV REM
%left EXCLAMATION



/*PRECEDENCE TOKENS*/

%left FLSUFFi
%left EXCOMMA_L 
%left EXCOMMA_H
%left ADDRESS ADDRMUT DEREF 
%left UMINUS NOT
%left LSQUARE RSQUARE
%left DOT
%precedence FNCALLEXP
%precedence FLUPSHIFT
%precedence NULLLIST
%left END

%%
program: crate { 

		$$ = $1;
		struct env *global;
		global = survey_defs($$);
		type_annotate($$, global);
		node_print($1);
		return 0;		 
		} 
crate:	 items {
		GNode * p = ast_node_new(CRATE, NULL);
		g_node_prepend(p, $1);	 	
		$$ = p;
		}

		/*===ITEMS===*/	
items:   item items { 
		g_node_prepend($2, $1);
		$$ = $2;
		}
|	 item {
		GNode * p = ast_node_new(ITEMS, NULL);
		g_node_prepend(p, $1);
		$$ = p;		
		}	
	
item:		fn-def 				{ $$ = $1; }
|		enum-def			{ $$ = $1; }
|		struct-def			{ $$ = $1; }


		/*FUNCTION DEF*/
fn-def: FN ID LPAREN RPAREN block {
			GNode * p = ast_node_new(FNDEF_ITEM, NULL);

			g_node_prepend(p, $5);

			GNode * ret_type = ast_node_new(FNDEFRET_ITEM, NULL);//TYPE_UNIT
			g_node_prepend(ret_type, ast_node_new(UNIT_TYPE, NULL));//TYPE_UNIT
			get_ast(ret_type)->type->kind = TYPE_UNIT;//TYPE_UNIT
			g_node_prepend(p, ret_type);//TYPE_UNIT

			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;
			}

|	FN ID LPAREN RPAREN FUNCTION_ARROW ret block {
			GNode * p = ast_node_new(FNDEF_ITEM, NULL);
			g_node_prepend(p, $7);
			g_node_prepend(p, $6);
			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;
			}
|	FN ID LPAREN fn-params RPAREN block {
			GNode * p = ast_node_new(FNDEF_ITEM, NULL);

			g_node_prepend(p, $6);

			GNode * ret_type = ast_node_new(FNDEFRET_ITEM, NULL);//TYPE_UNIT
			g_node_prepend(ret_type, ast_node_new(UNIT_TYPE, NULL));//TYPE_UNIT
			get_ast(ret_type)->type->kind = TYPE_UNIT;//TYPE_UNIT
			g_node_prepend(p, ret_type);//TYPE_UNIT

			g_node_prepend(p, $4);
			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;
			}
|	FN ID LPAREN fn-params RPAREN FUNCTION_ARROW ret block {
			GNode * p = ast_node_new(FNDEF_ITEM, NULL);
			g_node_prepend(p, $8);
			g_node_prepend(p, $7);
			g_node_prepend(p, $4);
			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;
			}
|	FN ID LPAREN fn-params RPAREN FUNCTION_ARROW EXCLAMATION block{
			GNode * p = ast_node_new(FNDEF_ITEM, NULL);

			g_node_prepend(p, $8);

			GNode * ret_type = ast_node_new(FNDEFRET_ITEM, NULL);//TYPE_DIV
			g_node_prepend(ret_type, ast_node_new(DIV_TYPE, NULL));//TYPE_DIV
			get_ast(ret_type)->type->kind = TYPE_DIV;//TYPE_DIV
			g_node_prepend(p, ret_type);//TYPE_DIV

			g_node_prepend(p, $4);
			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;

			}
|	FN ID LPAREN RPAREN FUNCTION_ARROW EXCLAMATION block{
			GNode * p = ast_node_new(FNDEF_ITEM, NULL);

			g_node_prepend(p, $7);

			GNode * ret_type = ast_node_new(FNDEFRET_ITEM, NULL);//TYPE_DIV
			g_node_prepend(ret_type, ast_node_new(DIV_TYPE, NULL));//TYPE_DIV
			get_ast(ret_type)->type->kind = TYPE_DIV;//TYPE_DIV
			g_node_prepend(p, ret_type);//TYPE_DIV

			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;
			}
fn-params:	fn-param COMMA fn-params {
			g_node_prepend($3, $1);
			$$ = $3;			
			}
|		fn-param {
			GNode * p = ast_node_new(FNDEFPARAMS_ITEM, NULL);
			g_node_prepend(p, $1);
			$$ = p;
			}
fn-param:	pat-type-annot {
			$$ = $1;		
			}
pat-type-annot:	pat COLON type {
			GNode * p = ast_node_new(FNDEFPARAM_ITEM, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p;			
			}
ret:		type {
			GNode * p = ast_node_new(FNDEFRET_ITEM, NULL);
			g_node_prepend(p, $1);
			$$ = p;	
		}
								
		/*ENUM DEF*/
enum-def:	ENUM ID LCURLY enum-ctor-defs RCURLY {
			GNode * p = ast_node_new(ENUMDEF_ITEM, NULL);
			g_node_prepend(p, $4);
			g_node_prepend(p, ast_node_new(ID, $2));			
			$$ = p;
			}
enum-ctor-defs: enum-ctor-def  COMMA  enum-ctor-defs {
			g_node_prepend($3, $1);
			$$ = $3;		
			}
|		enum-ctor-def {
			GNode * p = ast_node_new(ENUMDEFFIELDS_ITEM, NULL);
			g_node_prepend(p, $1);
			$$ = p;		
			}			
enum-ctor-def:  ID {
			GNode * p = ast_node_new(ENUMDEFFIELD_ITEM, NULL);
			g_node_prepend(p, ast_node_new(ID, $1));
			$$ = p;
			}				
|		ID LPAREN ctor-types RPAREN {
			GNode * p = ast_node_new(ENUMDEFFIELD_ITEM, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, ast_node_new(ID, $1));
			$$ = p;
			}				
ctor-types: 	type COMMA ctor-types {
			g_node_prepend($3, $1);
			$$ = $3;			
			}
|		type {
			GNode * p = ast_node_new(ENUMDEFFIELDTYPES_ITEM, NULL);
			g_node_prepend(p, $1);
			$$ = p;
			}


		/*STRUCT DEF*/
struct-def:	STRUCT ID LCURLY type-annots RCURLY {
			GNode * p = ast_node_new(STRUCTDEF_ITEM, NULL);
			g_node_prepend(p, $4);
			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;			
			}
type-annots:	type-annot COMMA type-annots {
			g_node_prepend($3, $1);
			$$ = $3;			
			}
|		type-annot {
			GNode * p = ast_node_new(STRUCTDEFFIELDS_ITEM, NULL);
			g_node_prepend(p, $1);
			$$ = p;
			}
type-annot: 	ID COLON type {
			GNode * p = ast_node_new(STRUCTDEFFIELD_ITEM, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, ast_node_new(ID, $1));
			$$ = p;			
			}

		/*BLOCKS*/
block:	/*%prec*/ 	LCURLY stmts RCURLY { $$ = $2; }			
|	/*%prec*/ 	LCURLY RCURLY {
			GNode * p = ast_node_new(BLOCK, NULL);
			$$ = p;
			}
/*|	 	LCURLY exp RCURLY {
			GNode * p = ast_node_new(BLOCK, NULL);
			g_node_prepend(p, $2);			
			$$ = p;
			}		*/		
		/*===STATEMENTS===*/
stmts:   	stmt stmts {
			g_node_prepend($2, $1);
			$$ = $2;
			}
|		stmt {
			GNode * p = ast_node_new(BLOCK, NULL);
			g_node_prepend(p, $1);				
			$$ = p;	
			}
| 		exp {
			GNode * p = ast_node_new(BLOCK, NULL);
			g_node_prepend(p, $1);
			$$ = p;
		}
stmt:    	exp SEMICOLON {
			GNode * p = ast_node_new(STMT, NULL);
			g_node_prepend(p, $1);
			$$ = p;	 
			}
|	 	RETURN exp SEMICOLON {
			GNode * p = ast_node_new(RETURNEXP_STMT, NULL);
			g_node_prepend(p, $2);
			$$ = p;				 
			}
|	 	RETURN SEMICOLON {
			GNode * p = ast_node_new(RETURN_STMT, NULL);
			$$ = p;			
			}
|	 	let SEMICOLON {
			$$ = $1;			
			}
//| 		exp 		{ $$ = $1; }	 
let:		LET pat {
			GNode * p = ast_node_new(LET_STMT, NULL);
			g_node_prepend(p, $2);
			$$ = p;
			}
|		LET pat COLON type ASSIGNMENT exp {
			GNode * p = ast_node_new(LETTYPEASS_STMT, NULL);
			g_node_prepend(p, $6);
			g_node_prepend(p, $4);
			g_node_prepend(p, $2);
			$$ = p;			
			}
|		LET pat ASSIGNMENT exp {
			GNode * p = ast_node_new(LETASS_STMT, NULL);
			g_node_prepend(p, $4);
			g_node_prepend(p, $2);
			$$ = p;	
			}
|		LET pat COLON type {
			GNode * p = ast_node_new(LETTYPE_STMT, NULL);
			g_node_prepend(p, $4);
			g_node_prepend(p, $2);
			$$ = p;	
			}


		/*===EXPRESSIONS===*/
exp : 	 	literal 			{$$ = $1;}
|	  	LPAREN exp RPAREN		{$$ = $2;}
|		ID				{$$ = ast_node_new(ID, $1); }
|		LPAREN RPAREN			{$$ = ast_node_new(UNIT_EXP, NULL);}
			/*OPERATIONS : NT's*/
|		or				{$$ = $1;}
|		and				{$$ = $1;}
|		eq				{$$ = $1;}
|		neq				{$$ = $1;}
|		lt				{$$ = $1;}
|		gt				{$$ = $1;}
|		geq				{$$ = $1;}
|		leq				{$$ = $1;}
|		add				{$$ = $1;}
|		sub				{$$ = $1;}
|		mul				{$$ = $1;}
|		div				{$$ = $1;}
|		rem				{$$ = $1;}
|		neg				{$$ = $1;}
|		not				{$$ = $1;}
|		deref				{$$ = $1;}
|		addr-of				{$$ = $1;}
|		addr-of-mut			{$$ = $1;}	
|		assign				{$$ = $1;}

			/*MISC : NT's*/
|		fn-call /*%prec FNCALLEXP*/	{$$ = $1;}
|		if				{$$ = $1;}
|		while				{$$ = $1;}
|		loop				{$$ = $1;}		
|		box-new				{$$ = $1;}
|	        arr-index			{$$ = $1;}
|		flup				{$$ = $1;}
|		match				{$$ = $1;}


			/*OPERATIONS : T's*/
or:		  exp BOOL_OR exp {
			GNode * p = ast_node_new(OR_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
and:	 	  exp BOOL_AND exp {
			GNode * p = ast_node_new(AND_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
eq:		  exp EQ exp {
			GNode * p = ast_node_new(EQ_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
neq:	 	  exp NE exp {
			GNode * p = ast_node_new(NE_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
lt:		  exp LT exp {
			GNode * p = ast_node_new(LT_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
gt:		  exp GT exp {
			GNode * p = ast_node_new(GT_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
leq:		  exp LE exp {
			GNode * p = ast_node_new(LE_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}	
geq:		  exp GE exp {
			GNode * p = ast_node_new(GE_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
add:		  exp ADD exp {
			GNode * p = ast_node_new(ADD_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
sub:	 	  exp NEGATIVE_SIGN exp {
			GNode * p = ast_node_new(SUB_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
mul:	       	  exp ASTERISK exp {
			GNode * p = ast_node_new(MUL_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
div:	       	  exp DIV exp {
			GNode * p = ast_node_new(DIV_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
rem:	      	  exp REM exp {
			GNode * p = ast_node_new(REM_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p; 
			}
neg:	      	  NEGATIVE_SIGN exp %prec UMINUS {
			GNode * p = ast_node_new(NEG_EXP, NULL);
			g_node_prepend(p, $2);
			$$ = p;			
			}
not:	   	  EXCLAMATION exp {
			GNode * p = ast_node_new(NOT_EXP, NULL);
			g_node_prepend(p, $2);
			$$ = p;	
			}
deref:	  	  ASTERISK exp	%prec DEREF {
			GNode * p = ast_node_new(DEREF_EXP, NULL);
			g_node_prepend(p, $2);
			$$ = p;
			}
addr-of:	  ADDRESS exp {
			GNode * p = ast_node_new(ADDR_EXP, NULL);
			g_node_prepend(p, $2);
			$$ = p;
			}
addr-of-mut:	  ADDRESS MUT exp  %prec ADDRMUT {
			GNode * p = ast_node_new(ADDRMUT_EXP, NULL);
			g_node_prepend(p, $3);
			$$ = p;
			}
   
			/*ASSIGNMENT : T's**/
assign  :       exp PLUS_ASSIGN exp                     {
                                                        GNode* p = ast_node_new(PLUS_ASSIGN, NULL);
                                                        g_node_prepend(p, $3);
                                                        g_node_prepend(p, $1);
                                                        $$ = p;
                                                        }
                
|               exp SUB_ASSIGN  exp                     {
                                                        GNode* p = ast_node_new(SUB_ASSIGN, NULL);
                                                        g_node_prepend(p, $3);
                                                        g_node_prepend(p, $1);
                                                        $$ = p;
                                                        }
|               exp MUL_ASSIGN  exp                     {
                                                        GNode* p = ast_node_new(MUL_ASSIGN, NULL);
                                                        g_node_prepend(p, $3);
                                                        g_node_prepend(p, $1);
                                                        $$ = p;
                                                        }

|               exp DIV_ASSIGN  exp                     {
                                                        GNode* p = ast_node_new(DIV_ASSIGN, NULL);
                                                        g_node_prepend(p, $3);
                                                        g_node_prepend(p, $1);
                                                        $$ = p;
                                                        }
|               exp REM_ASSIGN  exp                     {
                                                        GNode* p = ast_node_new(REM_ASSIGN, NULL);
                                                        g_node_prepend(p, $3);
                                                        g_node_prepend(p, $1);
                                                        $$ = p;
                                                        }
|               exp ASSIGNMENT  exp                     {
                                                        GNode* p = ast_node_new(ASSIGNMENT, NULL);
                                                        g_node_prepend(p, $3);
                                                        g_node_prepend(p, $1);
                                                        $$ = p;
                                                        }

			/*MISC : T's*/
fn-call:	ID LPAREN exps RPAREN {
			GNode * p = ast_node_new(FNCALL_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, ast_node_new(ID, $1));			
			$$ = p;
			}
|		ID LPAREN RPAREN {
			GNode * p = ast_node_new(FNCALL_EXP, NULL);
			GNode * c = ast_node_new(ID, $1);
			g_node_prepend(p, ast_node_new(FNCALLEMPTY_EXP, NULL));
			g_node_prepend(p, c);
			$$ = p;
			}
exps:		exp COMMA exps {
			g_node_prepend($3, $1);
			$$ = $3;
			}
|		exp {
			GNode * p = ast_node_new(FNCALLPARAMS_EXP, NULL);
			g_node_prepend(p, $1);
			$$ = p;
			}	
					
if:		IF LPAREN exp RPAREN block ELSE block   {
			GNode * p = ast_node_new(IF_EXP, NULL);
			g_node_prepend(p, $7);			
			g_node_prepend(p, $5);
			g_node_prepend(p, $3);
			$$ = p;   
			}
|		IF LPAREN exp RPAREN block {
			GNode * p = ast_node_new(IF_EXP, NULL);			
			g_node_prepend(p, $5);
			g_node_prepend(p, $3);
			$$ = p;
			}

while:		WHILE LPAREN exp RPAREN block {
			GNode * p = ast_node_new(WHILE_EXP, NULL);
			g_node_prepend(p, $5);
			g_node_prepend(p, $3);
			$$ = p;		
			}

loop: 		LOOP block {
			GNode * p = ast_node_new(LOOP_EXP, NULL);
			g_node_prepend(p, $2);
			$$ = p;		
			}

box-new:	BOX PATH NEW LPAREN exp RPAREN {
			GNode * p = ast_node_new(BOXNEW_EXP, NULL);
			GNode * c = ast_node_new(BOXNEWEXP_EXP, NULL);
			g_node_prepend(c, $5);			
			g_node_prepend(p, c);
			$$ = p;			
			}

arr-index: 	exp LSQUARE  exp  RSQUARE {
			GNode * p = ast_node_new(ARRIDX_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p;		
			}

flup: 	exp DOT exp    {
			GNode * p = ast_node_new(FIELDLUP_EXP, NULL);
	 		g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p;
			}

match:		MATCH LPAREN exp RPAREN LCURLY match-arms RCURLY {
			GNode * p = ast_node_new(MATCH_EXP, NULL);
			g_node_prepend(p, $6);
			g_node_prepend(p, $3);
			$$ = p;	
			}
match-arms:	match-arm COMMA match-arms {
			g_node_prepend($3, $1);
			$$ = $3;
			}
|               match-arm {
			GNode * p = ast_node_new(MATCHARMS_EXP, NULL);
			g_node_prepend(p, $1);
			$$ = p;				
			}   		
match-arm:	pat-or MATCH_ARROW block {
			GNode * p = ast_node_new(MATCHARM_EXP, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, $1);
			$$ = p;
			}	
pat-or:		pat BITOR pat-or {
			g_node_prepend($3, $1);
			$$ = $3;
			}
|		pat {
		  	GNode * p = ast_node_new(PATOR_EXP, NULL);
			g_node_prepend(p, $1);
			$$ = p;
			}



		/*===PATTERNS===*/
pat:		name-bind				{$$ = $1;}
|		prim-lit {
			GNode * p = ast_node_new(LIT_PAT, NULL);
			g_node_prepend(p, $1);
			$$ = p;
			}
|		NEGATIVE_SIGN {
			GNode * p = ast_node_new(LIT_PAT, NULL);
			g_node_prepend(p, ast_node_new(LITDEC, NULL)); 
			$$ = p;
			}
|		LPAREN RPAREN				{$$ = ast_node_new(UNIT_PAT, NULL);}
|		pat-deref 				{$$ = $1;}		
|		UNDERSCORE				{$$ = ast_node_new(UNDERSCORE, NULL);}
|		pat-arr					{$$ = $1;}
|		pat-enum				{$$ = $1;}		
|		pat-struct				{$$ = $1;}

name-bind:	ID {
			GNode * p = ast_node_new(ID_PAT, NULL);
			g_node_prepend(p, ast_node_new(ID, $1));
			$$ = p;		
		}
|		REF ID {
			GNode * p = ast_node_new(REF_PAT, NULL );
			g_node_prepend(p, ast_node_new(ID,$2));
			$$ = p;
		}		
|		REF MUT ID {
			GNode * p = ast_node_new(REFMUT_PAT, NULL);
			g_node_prepend(p, ast_node_new(ID, $3));
			$$ = p;		
		}
|		MUT ID {
			GNode * p = ast_node_new(MUT_PAT, NULL);
			g_node_prepend(p, ast_node_new(ID, $2));
			$$ = p;
		}

pat-deref:	ADDRESS pat {
			GNode * p = ast_node_new(DEREF_PAT, NULL);
			g_node_prepend(p, $2);
			$$ = p;			
			}

pat-arr:	LSQUARE  array-elmts  RSQUARE {
			GNode * p = ast_node_new(ARRAY_PAT, NULL);
			g_node_prepend(p, $2);
			$$ = p;			
			}
	
pat-enum:	ID PATH ID LPAREN ctor-elmts RPAREN {
			GNode * p = ast_node_new(ENUM_PAT, NULL);
			GNode * c = ast_node_new(ENUMCTOR_LIT, NULL);
			g_node_prepend(p, $5);
			g_node_prepend(p, c);
			g_node_prepend(c, ast_node_new(ID,$3));
			g_node_prepend(c, ast_node_new(ID,$1));
			$$ = p;
		}
|		ID PATH ID {
			GNode * p = ast_node_new(ENUM_PAT, NULL);
			GNode * c = ast_node_new(ENUMCTOR_LIT, NULL);
			g_node_prepend(p, c);
			g_node_prepend(c, ast_node_new(ID,$3));
			g_node_prepend(c, ast_node_new(ID,$1));
			$$ = p;	
}
			
pat-struct:	ID LCURLY id-pat-annots RCURLY {
			GNode * p = ast_node_new(STRUCT_PAT, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, ast_node_new(ID, $1));
			$$ = p;
		}


array-elmts: 	pat COMMA array-elmts {
			g_node_prepend($3, $1);
			$$ = $3;			
			}
|		pat {
			GNode * p = ast_node_new(ARRAYELMTS_PAT, NULL);
			g_node_prepend(p, $1);
			$$ = p;
			}

ctor-elmts: 	pat COMMA ctor-elmts {
			g_node_prepend($3, $1);
			$$ = $3;			
			}
|		pat {
			GNode * p = ast_node_new(ENUMFIELDS_PAT, NULL);
			g_node_prepend(p, $1);
			$$ = p; 
			}

id-pat-annots:  id-pat-annot COMMA id-pat-annots {
			g_node_prepend($3, $1);
			$$ = $3;		
		}
|		id-pat-annot {
			GNode * p = ast_node_new(STRUCTFIELDS_PAT, NULL);
			g_node_prepend(p, $1);
			$$ = p;
		}
id-pat-annot:	ID COLON pat {
			GNode * p = ast_node_new(STRUCTFIELD_PAT, NULL);
			g_node_prepend(p, $3);
			g_node_prepend(p, ast_node_new(ID, $1));
			$$ = p;	
		}

		/*===TYPES===*/
type: 		ID						{$$ = ast_node_new(ID, $1);} 
| 		BOOL 						{$$ = ast_node_new(BOOL, NULL);}
| 		U8 						{$$ = ast_node_new(U8, NULL);}
| 		I32 						{$$ = ast_node_new(I32, NULL);}
| 		LPAREN RPAREN					{$$ = ast_node_new(UNIT_TYPE, NULL);}
| 		LSQUARE type RSQUARE {
			GNode * p = ast_node_new(SLICE_TYPE, NULL);
			g_node_prepend(p, $2);
			$$ = p;			
			}
| 		LSQUARE type SEMICOLON LITDEC RSQUARE{
			GNode * p = ast_node_new(ARR_TYPE, NULL); 
			g_node_prepend(p, ast_node_new(LITDEC,&($4)));
			g_node_prepend(p, $2);
			$$ = p;
			}
| 		ADDRESS type{
			GNode * p = ast_node_new(ADDR_TYPE, NULL);
			g_node_prepend(p, $2);
			$$ = p; 
			}
| 		ADDRESS MUT type {
			GNode * p = ast_node_new(ADDRMUT_TYPE, NULL);
			g_node_prepend(p, $3);
			$$ = p;
			}
| 		BOX LT type GT {
			GNode * p = ast_node_new(BOX_TYPE, NULL);
			g_node_prepend(p, $3);
			$$ = p;			
			}


			/*===LITERALS===*/
literal:		prim-lit			{$$ = $1;}
|			comp-lit			{$$ = $1;}

prim-lit: 		lit-bool			{$$ = $1;}
|			LITCHAR				{$$ = ast_node_new(LITCHAR, NULL);}
|			LITDEC				{$$ = ast_node_new(LITDEC, &($1));}
|			LITSTR				{$$ = ast_node_new(LITSTR, NULL);}

//unit-lit:		LPAREN RPAREN			{$$ = ast_node_new(LIT_UNIT, NULL);}

comp-lit:		lit-enum			{$$ = $1;}
|			lit-struct			{$$ = $1;}
|			lit-array			{$$ = $1;}

lit-bool: TRU 	{ $$ = ast_node_new(TRU, &($1)); }
|	  FALS 	{ $$ = ast_node_new(FALS, &($1)); }
			
			/*ENUM LITERAL*/
lit-enum:		ID PATH ID {
				GNode * p = ast_node_new(ENUM_LIT, NULL);
				GNode * c = ast_node_new(ENUMCTOR_LIT, NULL);
				g_node_prepend(p, c);
				g_node_prepend(c, ast_node_new(ID, $3));
				g_node_prepend(c, ast_node_new(ID, $1));
				$$ = p;				
				}
|			ID PATH ID LPAREN enum-exps RPAREN {
				GNode * p = ast_node_new(ENUM_LIT, NULL);
				GNode * c = ast_node_new(ENUMCTOR_LIT, NULL);
				g_node_prepend(p, $5);
				g_node_prepend(p, c);
				g_node_prepend(c, ast_node_new(ID, $3));
				g_node_prepend(c, ast_node_new(ID, $1));
				$$ = p;	
				}
enum-exps:		exp COMMA enum-exps {
				g_node_prepend($3, $1);
				$$ = $3;
				}
|			exp {
				GNode * p = ast_node_new(ENUMFIELDS_LIT, $1);
				g_node_prepend(p, $1);
				$$ = p;
				}
	
			/*STRUCT LITERAL*/
lit-struct:		ID LCURLY id-exp-annots RCURLY {
				GNode * p = ast_node_new(STRUCT_LIT, NULL);
				g_node_prepend(p, $3);
				g_node_prepend(p, ast_node_new(ID, $1));
				$$ = p;			
				}
id-exp-annots:		id-exp-annot COMMA id-exp-annots {
				g_node_prepend($3, $1);
				$$ = $3;			
				}
|			id-exp-annot {
				GNode * p = ast_node_new(STRUCTFIELDS_LIT, NULL);
				g_node_prepend(p, $1);
				$$ = p;			
				}
id-exp-annot:		ID COLON exp {
				GNode * p = ast_node_new(STRUCTFIELD_LIT, NULL);
				g_node_prepend(p, $3);
				g_node_prepend(p, ast_node_new(ID, $1));
				$$ = p;			
				}

			/*ARRAY LITERAL*/
lit-array:		LSQUARE enum-exps RSQUARE {
				//printf("here\n");
				GNode * p = ast_node_new(ARRAY_LIT, NULL);
				//g_node_prepend(p, $2);

				g_node_unlink($2);
				GNode * child = ($2)->children;
				while(child){
					g_node_unlink(child);
					g_node_append(p, child);
					child = g_node_first_child($2);
				}
				g_node_destroy($2);

				$$ = p;
				}
%%

void yyerror(char const* msg){
	fprintf(stderr, "line:%d--Error: %s\n", yylval.line_number, msg);
	
}

YYSTYPE yylval = {{1,0,NULL,'a',0,NULL}};



