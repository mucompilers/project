#ifndef AST_H_
#define AST_H_
#include <glib.h>
#include <stdbool.h>


enum{
    AST_STMT_EXP,//0
    AST_UNIT,//1

    AST_TYPES,
    AST_STMTS,
    AST_EXPRS,
    AST_ID,//5
    
    AST_CRATE,
    AST_ITEMS,
    AST_FN_DEF1,
    AST_FN_DEF2,
    AST_ENUM_DEF,
    AST_STRUCT_DEF,
    AST_ENUM_CTOR_DEFS,
    AST_ENUM_CTOR_DEF,
    AST_ENUM_CTOR_PARAMS,
    AST_STRUCT_FIELD_DEFS,//15
    AST_STRUCT_FIELD_DEF,
    AST_FIELD_DEFS,
    AST_FIELD_DEF,
    AST_FN_PARAMS,
    AST_FN_PARAM,//20

    AST_TYPE_REF,//21
    AST_TYPE_REF_MUT,
    AST_TYPE_ARR1,
    AST_TYPE_ARR2,
    AST_TYPE_I32,
    AST_TYPE_U8,
    AST_TYPE_BOOL,
    AST_TYPE_BOX,
    AST_TYPE_UNIT,

    AST_PATS,//30
    AST_PAT_ARR,//31
    AST_PAT_STRUCT,
    AST_PAT_ENUM,
    AST_PAT_UNIT,
    AST_PAT_WILD,
    AST_PAT_DEREF,
    AST_PAT_ID,
    AST_PAT_REF_ID,
    AST_PAT_REF_MUT_ID,
    AST_PAT_MUT_ID,//40
    AST_PAT_LIT,//41
    AST_PAT_FIELD,
    AST_PAT_STRUCT_FIELDS,
    AST_PAT_ENUM_CTOR_PARAMS,
    AST_PAT_ARR_ELEMS,

    AST_BLOCK,
    AST_LET1,
    AST_LET2,
    AST_LET3,
    AST_LET4,//50
    AST_RETURN,//51

    AST_ENUM_CTOR,
    AST_STRUCT,
    AST_FIELD_LOOKUP,
    AST_ARR_INDEX,
    AST_FN_CALL1,
    AST_FN_CALL2,
    AST_ARR,
    AST_ASSIGN,
    AST_ASSIGN_SUB,//60
    AST_ASSIGN_ADD,//61
    AST_ASSIGN_MUL,
    AST_ASSIGN_DIV,
    AST_ASSIGN_REM,
    AST_OR,
    AST_AND,
    AST_EQ,
    AST_NEQ,
    AST_LT,
    AST_GT,//70
    AST_LEQ,//71
    AST_GEQ,
    AST_ADD,
    AST_SUB,
    AST_MUL,
    AST_DIV,
    AST_REM,
    AST_BOX_NEW,
    AST_PRINTLN,
    AST_MATCH,//80
    AST_IF1,//81
    AST_IF2,
    AST_WHILE,
    AST_LOOP,
    AST_NEG,
    AST_NOT,
    AST_DEREF,
    AST_ADDR_OF,
    AST_ADDR_OF_MUT,
    AST_FIELD_INITS,//90
    AST_FIELD_INIT,
    AST_MATCH_ARMS,
    AST_MATCH_ARM,
    AST_LIT_CHAR,
    AST_LIT_DEC,//95
    AST_LIT_STR,
    AST_TRUE,
    AST_FALSE,
};

struct ast{
    int kind;
    struct type* type;
    union{
	struct{
	    struct ast* una_node;
	};
	struct{
	    struct ast* bin_node1;
	    struct ast* bin_node2;
	};
	struct{
	    struct ast* ter_node1;
	    struct ast* ter_node2;
	    struct ast* ter_node3;
	};
	struct{
	    struct ast* quat_node1;
	    struct ast* quat_node2;
	    struct ast* quat_node3;
	    struct ast* quat_node4;
	};
	struct {
	    GList* list;
	};
	struct {
	    char* id;
	};
	struct {
	    char* lit_char;
	};
	struct {
	    int lit_dec;
	};
    };
};


struct ast* ast_unary(int, struct ast*);
struct ast* ast_binary(int , struct ast*, struct ast*);
struct ast* ast_ternary(int , struct ast* , struct ast*, struct ast*);
struct ast* ast_quater(int , struct ast*, struct ast*, struct ast*, struct ast*);
struct ast* ast_list(int, GList *);
struct ast* ast_list_gen(int , struct ast*);
struct ast* ast_id(char* );
struct ast* ast_lit_dec(int );
struct ast* ast_lit_char(char);
struct ast* ast_lit_str(char* );



#endif
