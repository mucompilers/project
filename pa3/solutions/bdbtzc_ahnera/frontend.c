#include <stdio.h>
#include <glib.h>
#include <assert.h>
#include <string.h>
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "type.h"
#include "env.h"
#include "ast_print.h"

static GList *crate;

struct env* glo_env;

void parse_done(GList *items) {
    crate = items;
}

void yyerror(char const *s) {
    fprintf(stderr, "%s\n", s);
}

static struct type *annotate_stmts(GList *stmts);

static struct type *annotate_exp(struct exp *exp);

struct type *annotate_binary(struct exp *exp);

struct type *annoatate_unary(struct exp *exp);

static struct env *build_env(GList *crate) {
    struct env *env = env_new();

    for (GList *i = g_list_first(crate); i; i = i->next) {
        assert(i->data);
        struct item *item = i->data;

        switch (item->kind) {
            case ITEM_FN_DEF: {
                if (env_contains(env, item->id)) {
                    printf("Error: duplicate definition of function `%s`.\n", symbol_to_str(item->id));
                    exit(1);
                }
                env_insert(env, item->id, item->fn_def.type);
                env_insert_def(env, item->id, item);

                GHashTable *params = g_hash_table_new(NULL, NULL);
                for (GList *j = g_list_first(item->fn_def.type->params); j; j = j->next) {
                    assert(j->data);
                    struct pair *pair = j->data;
                    assert(pair->kind == PAIR_PARAM);

                    assert(pair->param.pat->kind == PAT_BIND);

                    if (g_hash_table_lookup(params, GINT_TO_POINTER(pair->param.pat->bind.id.value))) {
                        printf("Error: identifier `%s` is bound more than once in the parameter list for the function `%s`.\n",
                               symbol_to_str(pair->param.pat->bind.id), symbol_to_str(item->id));
                        exit(1);
                    }
                    g_hash_table_insert(params, GINT_TO_POINTER(pair->param.pat->bind.id.value), GINT_TO_POINTER(true));
                }
                g_hash_table_destroy(params);

                break;
            }
            case ITEM_ENUM_DEF: {
                if (env_contains(env, item->id)) {
                    printf("Error: duplicate definition of struct or enum `%s`.\n", symbol_to_str(item->id));
                    exit(1);
                }
                env_insert_def(env, item->id, item);

                GHashTable *ctors = g_hash_table_new(NULL, NULL);
                for (GList *j = g_list_first(item->enum_def.ctors); j; j = j->next) {
                    assert(j->data);
                    struct pair *pair = j->data;
                    assert(pair->kind == PAIR_CTOR_DEF);

                    if (g_hash_table_lookup(ctors, GINT_TO_POINTER(pair->ctor_def.id.value))) {
                        printf("Error: duplicate definition of the constructor `%s` for the enum `%s`.\n",
                               symbol_to_str(pair->ctor_def.id), symbol_to_str(item->id));
                        exit(1);
                    }
                    g_hash_table_insert(ctors, GINT_TO_POINTER(pair->ctor_def.id.value), GINT_TO_POINTER(true));
                }
                g_hash_table_destroy(ctors);

                break;
            }
            case ITEM_STRUCT_DEF: {
                if (env_contains(env, item->id)) {
                    printf("Error: duplicate definition of struct or enum `%s`.\n", symbol_to_str(item->id));
                    exit(1);
                }
                env_insert_def(env, item->id, item);

                GHashTable *fields = g_hash_table_new(NULL, NULL);
                for (GList *j = g_list_first(item->struct_def.fields); j; j = j->next) {
                    assert(j->data);
                    struct pair *pair = j->data;
                    assert(pair->kind == PAIR_FIELD_DEF);

                    if (g_hash_table_lookup(fields, GINT_TO_POINTER(pair->field_def.id.value))) {
                        printf("Error: duplicate declaration of the field `%s` of the struct `%s`.\n",
                               symbol_to_str(pair->field_def.id), symbol_to_str(item->id));
                        exit(1);
                    }
                    g_hash_table_insert(fields, GINT_TO_POINTER(pair->field_def.id.value), GINT_TO_POINTER(true));
                }
                g_hash_table_destroy(fields);

                break;
            }
        }
    }

    return env;
}

static void check_main(struct env *env) {
    struct type *main_decl = env_lookup(env, symbol_main());

    if (main_decl->kind == TYPE_FN) {
        if (main_decl->params // it has params...
            || (main_decl->type && main_decl->type->kind != TYPE_UNIT)) { // ...or a non-unit return type.
            printf("Error: main function has the wrong type.\n");
            exit(1);
        }
    } else {
        printf("Error: main function not found.\n");
        exit(1);
    }
}

struct type *annotate_unary(struct exp *exp) {
    struct type *type = type_ok();

    switch (exp->kind) {
        case EXP_I32:
            type = type_error();
            break;
        case EXP_BINARY:
            type = annotate_exp(exp);
            if (type != type_bool())
                type = type_error();
            break;
        default:
            type = annotate_exp(exp);
            break;
    }

    return type;
}

struct type *annotate_binary(struct exp *exp) {
    struct type *type;

    if (exp_is_bool(exp)) {
        type = type_bool();
        if (annotate_exp(exp->binary.left) != type_bool())
            type = type_error();
        if (annotate_exp(exp->binary.right) != type_bool())
            type = type_error();
    } else if (exp_is_arith(exp)) {
        type = type_i32();
        if (annotate_exp(exp->binary.left) != type_i32())
            type = type_error();
        if (annotate_exp(exp->binary.right) != type_i32())
            type = type_error();
    } else if (exp_is_eq(exp)) {
        type = type_bool();
        if (annotate_exp(exp->binary.left) != annotate_exp(exp->binary.right))
            type = type_error();
    } else if (exp_is_compare(exp)) {
        type = type_bool();
        if (annotate_exp(exp->binary.left) != type_i32())
            type = type_error();
        if (annotate_exp(exp->binary.right) != type_i32())
            type = type_error();
    }

    return type;

}

struct type *annotate_exp(struct exp *exp) {
    assert(exp);

    struct type *type = NULL;

    switch (exp->kind) {
        case EXP_BLOCK:
            if (exp->block.stmts != NULL)
                type = annotate_stmts(exp->block.stmts);

            if (exp->block.exp != NULL) {
                if (type != type_error())
                    type = annotate_exp(exp->block.exp);
            }

            exp->type = type;
            break;
        case EXP_BINARY:
            type = annotate_binary(exp);
            exp->type = type;
            break;
        case EXP_UNARY:
            type = annotate_unary(exp->unary.exp);
            exp->type = type;
            break;
        case EXP_TRUE:
        case EXP_FALSE:
            type = type_bool();
            break;
        case EXP_I32:
            type = type_i32();
            break;
        case EXP_WHILE:
            type = type_unit();
            if (annotate_exp(exp->loop_while.cond) != type_bool())
                type = type_error();
            if (annotate_exp(exp->loop_while.block) != type_unit())
                type = type_error();
            exp->type = type;
            break;
        case EXP_LOOP:
            type = type_unit();
            if (exp->exp && annotate_exp(exp->exp) != type_unit())
                type = type_error();
            exp->type = type;
            break;
        case EXP_UNIT:
            type = type_unit();
            break;
        case EXP_ID:
            //printf("something\n\n");
            type = env_lookup(glo_env, type_id(exp->id)->id);
            exp->type = type;
            break;
    }

    return type;
}

struct type *annotate_stmts(GList *stmts) {

    struct type *type = type_ok();
    int has_error = 0;

    for (const GList *i = stmts; i; i = i->next) {
        struct stmt *stmt = i->data;

        assert(stmt);

        switch (stmt->kind) {
            case STMT_EXP:
                type = annotate_exp(stmt->exp);
                if (type == type_error())
                    has_error = 1;
                else
                    type = type_unit();

                stmt->type = type;
                break;
            case STMT_RETURN:
                type = annotate_exp(stmt->exp);
                if (type == type_error())
                    has_error = 1;

                stmt->type = type;
                break;
            case STMT_LET:
                type = annotate_exp(stmt->let.exp);
                /*if(!type_eq(type, stmt->let.type)) {
                    type = type_error();
                    has_error = 1;
                }*/
                stmt->type = type;
                break;
        }
    }

    if (has_error == 1)
        type = type_error();

    return type;
}


static void annotate_item(struct item *item, struct env *env) {
    assert(item);

    glo_env = env;

    switch (item->kind) {
        case ITEM_FN_DEF: {
            struct type *rtype = item->fn_def.type->type;
            if (annotate_exp(item->fn_def.block) == type_error() || annotate_exp(item->fn_def.block) != rtype)
                item->type = type_error();
            else
                item->type = type_ok();
            break;
        }
        case ITEM_ENUM_DEF:
            item->type = type_ok(); // nothing to check.
            break;
        case ITEM_STRUCT_DEF:
            item->type = type_ok(); // nothing to check.
            break;
    }
}

static void annotate_crate(GList *crate, struct env *env) {
    g_list_foreach(crate, (GFunc) annotate_item, env);
}

int main(int argc, char **argv) {
    if (!yyparse()) {
        struct env *genv = build_env(crate);

        check_main(genv);

        annotate_crate(crate, genv);

        crate_print(crate); // TODO (print pretty types)
        //puts("Ok.");
    }

    crate_destroy(crate);
    yylex_destroy();
}
