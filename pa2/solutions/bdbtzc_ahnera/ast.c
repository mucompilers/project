#include "ast.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


struct ast* ast_new_root(int kind, struct ast* left, struct ast* right){
    struct ast *ast = malloc(sizeof *ast);
    ast->kind = kind;
    ast->left = left;
    ast->right = right;

    return ast;
}

void ast_add_param(struct ast* parent, struct ast* param){
    parent->params = g_list_append(parent->params, param);
}

struct ast* ast_new_val(int kind, char * value){
    struct ast *ast = malloc(sizeof *ast);
    ast->kind = kind;
    ast->value = value;

    return ast;
}

void ast_print(struct ast *ast) {
    if (!ast) return;

    int i;

    if (count != 0) {
        printf("\n");
        for (i = 0; i < count; i++) {
            printf("\t");
        }
    }

    printf("(");

    count++;

    switch (ast->kind) {
        case AST_FN_DEF:
            printf("fn-def");
            break;
        case AST_ID:
            printf("id");
            break;
        case AST_CRATE:
            printf("crate");
            break;
        case AST_ITEMS:
            printf("items");
            break;
        case AST_ADD:
            printf("add");
            break;
        case AST_SUB:
            printf("sub");
            break;
        case AST_MUL:
            printf("mul");
            break;
        case AST_DIV:
            printf("div");
            break;
        case AST_MOD:
            printf("mod");
            break;
        case AST_OR:
            printf("or");
            break;
        case AST_AND:
            printf("and");
            break;
        case AST_EQ:
            printf("eq");
            break;
        case AST_LT:
            printf("lt");
            break;
        case AST_GT:
            printf("gt");
            break;
        case AST_LTE:
            printf("leq");
            break;
        case AST_GTE:
            printf("geq");
            break;
        case AST_NEG:
            printf("neg");
            break;
        case AST_VAL:
            printf("%s", ast->value);
            break;
        case AST_INVALID:
            printf("ERROR: Syntax not supported.\n");
            exit(1);
    }

    ast_print(ast->left);
    ast_print(ast->right);
    printf(")");
    count--;
}
