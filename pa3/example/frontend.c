#include <stdio.h>
#include <glib.h>
#include <assert.h>
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "type.h"
#include "env.h"
#include "ast_print.h"

static GList* crate;

void parse_done(GList* items) {
      crate = items;
}

void yyerror(char const* s) {
      fprintf(stderr, "%s\n", s);
}

static struct env* build_env(GList* crate) {
      struct env* env = env_new();

      for (GList* i = g_list_first(crate); i; i = i->next) {
            assert(i->data);
            struct item* item = i->data;

            switch (item->kind) {
                  case ITEM_FN_DEF: {
                        if (env_contains(env, item->id)) {
                              printf("Error: duplicate definition of function `%s`.\n", symbol_to_str(item->id));
                              exit(1);
                        }
                        env_insert(env, item->id, item->fn_def.type);
                        env_insert_def(env, item->id, item);

                        GHashTable* params = g_hash_table_new(NULL, NULL);
                        for (GList* j = g_list_first(item->fn_def.type->params); j; j = j->next) {
                              assert(j->data);
                              struct pair* pair = j->data;
                              assert(pair->kind == PAIR_PARAM);

                              assert(pair->param.pat->kind == PAT_BIND);
                              
                              if (g_hash_table_lookup(params, GINT_TO_POINTER(pair->param.pat->bind.id.value))) {
                                    printf("Error: identifier `%s` is bound more than once in the parameter list for the function `%s`.\n", symbol_to_str(pair->param.pat->bind.id), symbol_to_str(item->id));
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

                        GHashTable* ctors = g_hash_table_new(NULL, NULL);
                        for (GList* j = g_list_first(item->enum_def.ctors); j; j = j->next) {
                              assert(j->data);
                              struct pair* pair = j->data;
                              assert(pair->kind == PAIR_CTOR_DEF);

                              if (g_hash_table_lookup(ctors, GINT_TO_POINTER(pair->ctor_def.id.value))) {
                                    printf("Error: duplicate definition of the constructor `%s` for the enum `%s`.\n", symbol_to_str(pair->ctor_def.id), symbol_to_str(item->id));
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

                        GHashTable* fields = g_hash_table_new(NULL, NULL);
                        for (GList* j = g_list_first(item->struct_def.fields); j; j = j->next) {
                              assert(j->data);
                              struct pair* pair = j->data;
                              assert(pair->kind == PAIR_FIELD_DEF);

                              if (g_hash_table_lookup(fields, GINT_TO_POINTER(pair->field_def.id.value))) {
                                    printf("Error: duplicate declaration of the field `%s` of the struct `%s`.\n", symbol_to_str(pair->field_def.id), symbol_to_str(item->id));
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

static void check_main(struct env* env) {
      struct type* main_decl = env_lookup(env, symbol_main());

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

static void annotate_item(struct item* item, struct env* env) {
      assert(item);

      switch (item->kind) {
            case ITEM_FN_DEF:
                  // ... TODO annotate the body TODO ...
                  break;

            case ITEM_ENUM_DEF:
                  item->type = type_ok(); // nothing to check.
                  break;
            case ITEM_STRUCT_DEF:
                  item->type = type_ok(); // nothing to check.
                  break;
      }
}

static void annotate_crate(GList* crate, struct env* env) {
      g_list_foreach(crate, (GFunc)annotate_item, env);
}

int main(int argc, char** argv) {
      if (!yyparse()) {
            struct env* genv = build_env(crate);

            check_main(genv);

            annotate_crate(crate, genv);
            
            // crate_print(crate); // TODO (print pretty types)
            puts("Ok.");
      }

      crate_destroy(crate);
      yylex_destroy();
}
