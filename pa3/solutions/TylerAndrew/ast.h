#ifndef AST_H_
#define AST_H_

#include <glib.h>

enum {
        AST_INVALID = 450,
	
	/*LITERALS*/
      	ARRAY_LIT,

	ENUM_LIT,
	ENUMCTOR_LIT,
	ENUMFIELDS_LIT,

	STRUCT_LIT,
	STRUCTFIELDS_LIT,
	STRUCTFIELD_LIT,
	
	/*type annotation utility*/
	OKAY,
	UNTYPED,

	/*TYPES*/
	UNIT_TYPE,

	SLICE_TYPE,

	ARR_TYPE,

	ARRSIZE_TYPE,

	ADDR_TYPE,

	ADDRMUT_TYPE,

	BOX_TYPE,

	DIV_TYPE,
	
	/*PATTERNS*/
	PAT,

	NAMEBIND_PAT,
	ID_PAT,
	REF_PAT,
	REFMUT_PAT,
	MUT_PAT,

	UNIT_PAT,

	LIT_PAT,

	DEREF_PAT,

	ARRAY_PAT,

	ARRAYELMTS_PAT,

	ENUM_PAT,
	ENUMFIELDS_PAT,
	ENUMFIELD_PAT,
	
	STRUCT_PAT,
	STRUCTFIELDS_PAT,
	STRUCTFIELD_PAT,
	
	PATLIST_PAT,

	/*EXPRESSIONS*/
	EXP,
	
	UNIT_EXP,
 
	OR_EXP,
	AND_EXP,
	EQ_EXP,
	NE_EXP,
	LT_EXP,
	GT_EXP,
	LE_EXP,
	GE_EXP,
	ADD_EXP,
	SUB_EXP,
	MUL_EXP,
	DIV_EXP,
	REM_EXP,
	NEG_EXP,
	NOT_EXP,
	DEREF_EXP,
        ADDR_EXP,
	ADDRMUT_EXP,
		
	FNCALL_EXP,
	FNCALLEMPTY_EXP,
	FNCALLPARAMS_EXP,
	FNCALLPARAM_EXP,		
	
	IF_EXP,
	IFEVAL_EXP,
	IFBLOCK_EXP,
	ELSEBLOCK_EXP,
	
	WHILE_EXP,
	WHILEEVAL_EXP,
	WHILEBLOCK_EXP,		

	LOOP_EXP,
	LOOPBLOCK_EXP,	

	BOXNEW_EXP,
	BOXNEWEXP_EXP,

	ARRIDX_EXP,
	ARRIDXEXP_EXP,	
		
	FIELDLUP_EXP,
	FIELD_EXP,
	
	MATCH_EXP,
	MATCHEXP_EXP,
	MATCHARMS_EXP,
	MATCHARM_EXP,
	PATOR_EXP,
	MATCHBLOCK_EXP,
	
	/*STATEMENTS*/
	STMTS,	
	STMT,
	
	LET_STMT,
	LETTYPEASS_STMT,
	LETASS_STMT,
	LETTYPE_STMT,

	RETURN_STMT,
	RETURNEXP_STMT,


	/*BLOCKS*/	
	BLOCK,
	LOOPBLOCK,

	/*ITEMS*/
	ITEM,
	ITEMS,
	ENUMDEF_ITEM,
	ENUMDEFFIELDS_ITEM,
	ENUMDEFFIELD_ITEM,
	ENUMDEFFIELDTYPES_ITEM,
	
	STRUCTDEF_ITEM,
	STRUCTDEFFIELDS_ITEM,
	STRUCTDEFFIELD_ITEM,	

	FNDEF_ITEM,
	FNDEFPARAMS_ITEM,
	FNDEFPARAM_ITEM,
	FNDEFRET_ITEM,
	FNDEFBLOCK_ITEM
	
};



struct ast {
 /*
      int super_kind;	// The general kind (ie pattern, expression, statement, definition)
      int kind;		// The specific kind of node
      int type;		// Its type
      int length;
      int depth;
      char * typeid;
*/
      int kind;
      struct type * type;
     
     
      union {		//values of an expression
	int num;	// i32
	char * str;	// literal string or ID
	char c;		// u8
      };

      GHashTable *fields;	// used to lookup fields and parameters for definitions

};


GNode * ast_node_new(int, void *);
void type_print(struct type *);
void node_print(GNode *);
void print_struct_enum(struct type*);
#endif
