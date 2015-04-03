//#ifnde
#define AST_H_
#include <glib.h>

enum {
	AST_FN_CALL1,
    	AST_ADD,
	AST_SUB,
	AST_MUL,
	AST_DIV,
	AST_REM,
	AST_NEG,
	AST_AND,
	AST_OR,
	AST_NOT,
	AST_LT,
	AST_GT,
	AST_LEQ,
	AST_GEQ,
	AST_EQ,
	AST_NEQ,
	AST_ID,
	AST_LIT_DEC,
	AST_ADDR_OF,
	AST_ADDR_OF_MUT,
	AST_DEREF,
	AST_FIELD_LOOKUP,
	AST_ARR_INDEX,
	AST_ASSIGN,
	AST_ASSIGN_ADD,
	AST_ASSIGN_SUB,
	AST_ASSIGN_MUL,
	AST_ASSIGN_DIV,
	AST_ASSIGN_REM,
	AST_BLOCK,
	AST_WHILE,
	AST_LOOP,
	AST_IF1,
	AST_IF2,
	AST_MATCH,
	AST_MATCH_ARM,
	AST_LIT_CHAR,
	AST_LIT_STR,
	AST_TRUE,
	AST_FALSE,
	AST_UNIT,
	AST_ENUM_CTOR,
	AST_ENUM_CTOR_PARAMS,
	AST_ENUM,
	AST_STRUCT,
	AST_FIELD_INIT,
	AST_ARR,
	AST_EXPRS,
	AST_PAT_ENUM,
	AST_PAT_ENUM_CTOR_PARAMS,
	AST_PAT_LIT,
	AST_PAT_ID,
	AST_PAT_REF_ID,
	AST_PAT_REF_MUT_ID,
	AST_PAT_MUT_ID,
	AST_PAT_DEREF,
	AST_PAT_ARR,
	AST_PAT_ELEM,
	AST_PAT_STRUCT,
	AST_PAT_FIELD,
      	AST_PAT_WILD,	
	AST_LET1,
	AST_LET2,
	AST_LET3,
	AST_LET4,
	AST_RETURN1,
	AST_RETURN2,
	AST_TYPE_ARR1,
	AST_TYPE_ARR2,
	AST_TYPE_REF,
	AST_TYPE_MUT_REF,
	AST_TYPE_BOX,
	AST_TYPE_UNIT,
	AST_TYPE_BOOL,
	AST_TYPE_U8,
	AST_TYPE_I32,
	AST_FN_CALL,
	AST_BOX_NEW,
	AST_EMPTY,
	AST_UNIT_EXP,
	AST_CRATE,
	AST_FN_DEF1,
	AST_FN_DEF2,
	AST_FN_DEF3,
	AST_FN_DEF4,
	AST_FN_DEF5,
	AST_FN_PARAM,
	AST_ENUM_DEF,
	AST_ENUM_CTOR_DEF1,
	AST_ENUM_CTOR_DEF2,
	AST_STRUCT_DEF,
	AST_FIELD_DEF,
	AST_PAT_UNIT,
};

struct ast {
      int kind;
      union {
	    struct {
		struct ast* left;
		struct ast* right;
	    };
	    struct {
		struct ast* child;
	    };
	    struct {
		char* id_child;
	    };
	    struct {
		int  lit_dec_child;
	    };
	    struct {
		char lit_char_child;
	    };
	    struct {
		struct ast* while_left;
		GList* while_right;
	    };
	    struct {
		GList* loop;
	    };
	    struct {
		struct ast* if_condition;
		GList* if_body;
		GList* else_body;
	    };
	    struct {
		struct ast* match_exp;
		GList * match_body;
	    };
	    struct { 
		struct ast* triop_1;
		struct ast* triop_2;
		struct ast* triop_3;
	    };
	    struct {
		struct ast* biquop_a1;
		GList* biquop_g1;
		struct ast* biquop_a2;
		GList* biquop_g2;
	    };
	    struct {
		struct ast* triop_llg1;
		struct ast* triop_llg2;
		GList * triop_llg3;
	    };
      };
};


struct ast* ast_biquop_lglg(int, struct ast*, GList *,struct ast*, GList *);
struct ast* ast_triop_lgg(int, struct ast*, GList *, GList*);
struct ast* ast_triop_llg(int, struct ast*, struct ast*, GList*);




struct ast* ast_triop_new(int, struct ast*, struct ast*, struct ast*);
struct ast* ast_empty_new(int );

struct ast* ast_binop_new(int, struct ast*, struct ast* );
struct ast* ast_unaop_new(int, struct ast*);
struct ast* ast_lit_dec_new(int);
struct ast* ast_id_new(char*);
struct ast* ast_while_new(struct ast*, GList *);
struct ast* ast_loop_new(GList *);
struct ast* ast_if_new1(struct ast*, GList *);
struct ast* ast_if_new2(struct ast* ,GList *, GList *);
struct ast* ast_match_new(struct ast* , GList *);
struct ast* ast_bool_new(int );
struct ast* ast_lit_char_ne(char);
struct ast* ast_lit_str_new(char *);
struct ast* ast_lit_dec_new(int );
struct ast* ast_unit_new();
struct ast* ast_enum_ctor_params(struct ast *, GList *);
struct ast* ast_arr_new(GList *);
struct ast* ast_pat_elems_new(GList *);
struct ast* ast_pat_enum_new(struct ast *, GList*);
struct ast* ast_pat_wild_new();

struct ast* ast_binop_lg(int kind, struct ast*, GList*);
struct ast* ast_unaop_g(int kind, GList*);
struct ast* ast_binop_gg( int kind, GList *, GList* );
struct ast* ast_block_new();

void ast_print(struct ast*, int );
void indent(int);
void ast_print1(struct ast* , int);
struct ast* ast_lit_char_new(char);
//#endif
