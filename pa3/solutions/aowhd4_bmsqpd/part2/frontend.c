#include <stdio.h>
#include <glib.h>
#include <assert.h>
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "type.h"
#include "env.h"
#include "ast_print.h"

//prototypes
static struct type* annotate_block(struct exp* block, struct env* env);
static struct type* annotate_array(GList* exps, struct env* env);
static void annotate_stmt(struct stmt* stmt, struct env* env);
static struct type* annotate_exp(struct exp* exp, struct env* env);
static void check_block(struct stmt* stmt, bool* is_error);
static struct type* check_bin(struct type* left, struct type* right, const char* op, struct env* env);
static struct type* check_unary(const char* op, struct type* type, struct env* env);

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

static struct type* annotate_array(GList* exps, struct env* env) {
	//TODO
	return type_unit();
}

static struct type* check_bin(struct type* left, struct type* right, const char* op, struct env* env) {
	
	if (type_eq(left, right)) {
		if (!strcmp(op, "=") || !strcmp(op, "-=") || !strcmp(op, "+=") || !strcmp(op, "*=") || !strcmp(op, "/=") || !strcmp(op, "%=") || !strcmp(op, "+") || !strcmp(op, "-") || !strcmp(op, "*") || !strcmp(op, "/") || !strcmp(op, "%")) {
			return type_copy(left);
		} else {
			return type_bool();
		}
	} else {
		return type_error();
	}
}

static struct type* check_unary(const char* op, struct type* type, struct env* env) {
		
	if (!strcmp(op, "&")) {
		//if (type_eq(type, type_i32()) || type_eq(type, type_u8()))
	}
    if (!strcmp(op, "!")) {
    	if (type_eq(type, type_bool())) {
    		return type;
    	} else {
    		return type_error();
    	}
    }
  	if (!strcmp(op, "*")) {
  	
  	}
  	if (!strcmp(op, "-")) {
  	
  	}
}

static struct type* annotate_exp(struct exp* exp, struct env* env) {

	switch (exp->kind) {
		case EXP_INVALID:
			printf("1\n");
			exp->type = type_invalid();
			return exp->type;
			break;
	  	case EXP_U8:
	  		printf("2\n");
	  		exp->type = type_u8();
	  		return exp->type;
	  		break;
	  	case EXP_I32:
	  		printf("3\n");
	  		exp->type = type_i32();
	  		return exp->type;
	  		break;
	  	case EXP_TRUE:
	  		printf("4\n");
	  		exp->type = type_bool();
	  		return exp->type;
	  		break;
	  	case EXP_FALSE:
	  		printf("5\n");
	  		exp->type = type_bool();
	  		return exp->type;
	  		break;
	  	case EXP_STR:
	  		printf("6\n");
	  		break;
	  	case EXP_UNIT:
	  		printf("7\n");
	  		exp->type = type_unit();
	  		return exp->type;
	  		break;
	  	case EXP_ID:
	  		printf("8\n");
	  		exp->type = type_copy(env_lookup(env, exp->id));
	  		return exp->type;
	  		break;
	  	case EXP_ENUM:
	  		printf("9\n");
	  		break;
	  	case EXP_STRUCT:
	  		printf("10\n");
	  		break;
	  	case EXP_LOOKUP:
	  		printf("11\n");
	  		break;
	  	case EXP_INDEX:
	  		printf("12\n");
	  		exp->type = annotate_exp(exp->index.exp, env);
	  		exp->type = annotate_exp(exp->index.idx, env);
	  		break;
	  	case EXP_FN_CALL:
	  		printf("13\n");
	  		break;
	  	case EXP_ARRAY:
	  		printf("14\n");
	  		exp->type = annotate_array(exp->lit_array.exps, env);
	  		return exp->type;
	  		break;
	  	case EXP_BOX_NEW:
	  		printf("15\n");
	  		break;
	  	case EXP_MATCH:
	  		printf("16\n");
	  		break;
	  	case EXP_IF:
	  		printf("17\n");
	  		break;
	  	case EXP_WHILE:
	  		printf("18\n");
	  		exp->loop_while.cond->type = annotate_exp(exp->loop_while.cond, env);
            exp->loop_while.block->type = annotate_exp(exp->loop_while.block, env);
            if (!type_eq(exp->loop_while.cond->type, type_bool())) {
	  			exp->type = type_error();
	  			return type_error();
	  		} else if (!type_eq(exp->loop_while.block->type, type_unit())) {
	  			exp->type = type_error();
	  			return type_error();
	  		} else {
	  			exp->type = type_unit();
	  			return type_unit();
	  		}
	  		break;
	  	case EXP_LOOP:
	  		printf("19\n");
	  		exp->type = annotate_exp(exp->exp, env);
	  		if (!type_eq(exp->type, type_unit())) {
	  			exp->type = type_error();
	  			return type_error();
	  		} else {
	  			return type_unit();
	  		}
	  		break;
	  	case EXP_BLOCK:
	  		printf("20\n");
	  		exp->type = annotate_block(exp, env);
	  		if (type_eq(exp->type, type_error())) {
	  			return type_error();
	  		} else {
	  			return exp->type;
	  		}
	  		break;
	  	case EXP_UNARY:
	  		printf("21\n");
	  		exp->unary.exp->type = annotate_exp(exp->unary.exp, env);
	  		exp->type = check_unary(exp->unary.op, exp->unary.exp->type, env);
	  		if (type_eq(exp->type, type_error())) {
	  			return type_error();
	  		} else {
	  			return exp->type;
	  		}
	  		break;
	  	case EXP_BINARY:
	  		printf("22\n");
	  		exp->binary.left->type = annotate_exp(exp->binary.left, env);
	  		exp->binary.right->type = annotate_exp(exp->binary.right, env);
	  		exp->type = check_bin(exp->binary.left->type, exp->binary.right->type, exp->binary.op, env);
	  		if (type_eq(exp->type, type_error())) {
	  			return type_error();
	  		} else {
	  			return exp->type;
	  		}
	  		break;
	  	default:
	  		printf("def\n");
	  		break;
	}
/*	if (is_error) {
		return type_error();
	}*/
}

static void annotate_stmt(struct stmt* stmt, struct env* env) {
	switch (stmt->kind) {
		case STMT_INVALID:
			printf("invalid\n");
			break;
      	case STMT_LET:
      		printf("let\n");
      		env_insert(env, stmt->let.pat->bind.id, stmt->let.type);
      		struct type* temp = annotate_exp(stmt->let.exp, env);
      		if (type_eq(temp, stmt->let.type)) {
      			stmt->type = type_unit();
      		} else {
      			stmt->type = type_error();
      		}
      		break;
      	case STMT_RETURN:
      		printf("return\n");
      		stmt->type = annotate_exp(stmt->exp, env);
      		if (type_eq(stmt->type, type_error())) {
      			stmt->type = type_error();
      		} else {
      			stmt->type = type_unit();
      		}
      		break;
      	case STMT_EXP:
      		stmt->type = annotate_exp(stmt->exp, env);
      		if (type_eq(stmt->type, type_error())) {
      			stmt->type = type_error();
      		} else {
      			stmt->type = type_unit();
      		}
      		break;
      	default:
      		printf("def\n");
      		break;
	}
}

static void check_block(struct stmt* stmt, bool* is_error) {
	if (type_eq(stmt->type, type_error())) {
		*is_error = true;
	}
}

static struct type* annotate_block(struct exp* block, struct env* env) {

	if (block->block.stmts == NULL) {
		printf("exp not null\n");
		block->type = annotate_exp(block->block.exp, env);
		if (type_eq(block->type, type_error())) {
      		block->type = type_error();
      		return type_error();
      	} else {
      		return block->type;
      	}
	} else {
		g_list_foreach((GList*)block->block.stmts, (GFunc)annotate_stmt, env);
		bool is_error = false;
		g_list_foreach((GList*)block->block.stmts, (GFunc)check_block, &is_error);
		
		block->block.exp->type = annotate_exp(block->block.exp, env);
		
		if (is_error) {
			block->type = type_error();
			return type_error();
		} else {
			block->type = block->block.exp->type;
			return block->type;
		}
	}
}

static void annotate_item(struct item* item, struct env* env) {
      assert(item);

      switch (item->kind) {
            case ITEM_FN_DEF:
            	  item->fn_def.block->type = annotate_block(item->fn_def.block, env);
            	  if (type_eq(item->type, type_unit())) {
            	  	item->type = type_ok();
            	  } else if (!type_eq(item->type, item->fn_def.type)){
            	  	item->type = type_error();
            	  } else {
            	  	item->type = type_error();
            	  }
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
            
            crate_print(crate); // TODO (print pretty types)
      }

      crate_destroy(crate);
      yylex_destroy();
}
