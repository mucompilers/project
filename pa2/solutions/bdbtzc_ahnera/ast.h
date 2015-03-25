#ifndef AST_H_
#define AST_H_
#include <glib.h>

enum {
    AST_INVALID,
    AST_LET,
    AST_RETURN,
    AST_TRUE,
    AST_FALSE,
    AST_STRUCT,
    AST_MATCH,
    AST_IF,
    AST_WHILE,
    AST_DEREF,
    AST_CRATE,
    AST_ITEMS,
    AST_FN_DEF,
    AST_ENUM_DEF,
    AST_BLOCK,
    AST_ID,
    AST_VAL,
    AST_ADD,
    AST_SUB,
    AST_MUL,
    AST_DIV,
    AST_MOD,
    AST_OR,
    AST_AND,
    AST_EQ,
    AST_LT,
    AST_GT,
    AST_LTE,
    AST_GTE,
    AST_NEG,
};

struct ast {
    int kind;
    char *value;
    struct ast *left;
    struct ast *right;
    GList *params;
    GList *block;
};

struct ast* ast_new_root(int, struct ast*, struct ast*);

void ast_add_param(struct ast* parent, struct ast* param);

struct ast* ast_new_val(int kind, char * value);

void ast_print(struct ast *);

int count, started;

struct ast *list;

#endif
