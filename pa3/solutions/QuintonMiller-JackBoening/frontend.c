#include <stdio.h>
#include <glib.h>
#include <assert.h>
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "type.h"
#include "env.h"
#include "ast_print.h"

struct type* get_type_crate(GList* items);
struct type* get_type_stmt(struct stmt* stmt, struct type* expected);
struct type* get_type_exp(struct exp* exp);
struct type* get_type_item(struct item* item);
struct type *nearest_type(Symbol s);
GList* add_environment();
void delete_environment();
struct env* get_first_environment();
static GList* crate;
GList* envs;

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

      if (/*main_decl->kind == TYPE_FN*/true) {
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

            //annotate_crate(crate, genv);
            
            crate_print(crate); // TODO (print pretty types)
            //puts("Ok.");
      }

      crate_destroy(crate);
      yylex_destroy();
}

struct type* get_type_crate(GList* items){
  struct type* ret = type_ok();
  struct item* item;
  GList *it = items;
  
  for (; it != NULL; it = it->next){
    item = it->data;
    item->type = get_type_item(item);
    if (ret->kind != TYPE_ERROR && item->type->kind != TYPE_OK)
      ret = type_error();
  }
  
  return ret;
}

struct type* get_type_block(struct exp* block, struct type* type){
  add_environment();
  //struct env* lEnv = get_first_environment();
  GList *stmts = block->block.stmts;          // Block's statements
  GList *it = stmts;                          // Statements iterator
  struct exp *expression = block->block.exp;  // Block's final expression
  struct stmt *stmt;                          // Current statement
  struct type *ret = type_unit();             // Return type
  
  // If type is null, use type-unit
  if (!type)
    type = type_unit();
  
  // Loop thru statements
  for (; it != NULL; it = it->next){
    
    // Current statement
    stmt = it->data;  
    
    // Get statement's type
    stmt->type = get_type_stmt(stmt, type);
    
    // IF return type is not type_error AND either
    //    this statement is type error
    // OR this return does not match function type
    // THEN type_error
    if (ret->kind != TYPE_ERROR){
      if (stmt->type->kind == TYPE_ERROR){
        ret = type_error();
      }
    }
    
    if (stmt->kind == STMT_RETURN)
      if (stmt->type->kind == TYPE_UNIT)
        ret = type_ok();
    
  }
  
  // If there is a final expression, get its type
  if (expression)
    expression->type = get_type_exp(expression);
  else
    expression->type = type_unit();
  
  delete_environment();
  
  if (ret->kind == TYPE_OK){
    return type_ok();
  }
  
  // If haven't reached an error, return the type of the expression
  if (ret->kind != TYPE_ERROR)
    return expression->type;
  else
    return type_error();
  
}

struct type *get_type_stmt(struct stmt* stmt, struct type *expected){

  struct type *ret = type_error();
  
  switch (stmt->kind) {
      case STMT_LET:
        // Type-unit if either
        //    type AND exp match
        // OR type is NULL and exp is not ERROR
        // ELSE error
        
        stmt->let.exp->type = get_type_exp(stmt->let.exp);
        if ((!stmt->let.type && stmt->let.exp->type->kind != TYPE_ERROR) || 
            (stmt->let.type && stmt->let.type->kind == stmt->let.exp->type->kind))
          ret = type_unit();
    
        if (envs){
          if (ret->kind == TYPE_UNIT){
            if (stmt->let.type){
              env_insert(get_first_environment(), stmt->let.pat->bind.id, stmt->let.type);
            }
            else{
              env_insert(get_first_environment(), stmt->let.pat->bind.id, stmt->let.exp->type); 
            }
          }
        }
        
        break;
      case STMT_RETURN:
        // Type-unit if exp's type matches block's type
        // else type-error
    
        stmt->exp->type = get_type_exp(stmt->exp);
        if (stmt->exp->type->kind == expected->kind)
          ret = type_unit();
      
        break;
      case STMT_EXP:
        // Type-unit if expression is not type-error
        // else type-error
    
        stmt->exp->type = get_type_exp(stmt->exp);
        if (stmt->exp->type->kind != TYPE_ERROR)
          ret = type_unit();
    
        break;
  }
  
  
  return ret;
}

struct type* get_type_item(struct item* item){
  struct type* ret = type_ok();

  if (item->kind == ITEM_FN_DEF){
    item->fn_def.block->type = get_type_block(item->fn_def.block, item->fn_def.ret);
    if (item->fn_def.block->type->kind == TYPE_OK){
      item->fn_def.block->type = type_unit();
      ret = type_ok();
    }
    else{
      if (item->fn_def.block->type->kind != item->fn_def.ret->kind)
        ret = type_error();
      if (item->fn_def.block->type->kind == TYPE_ERROR)
        ret = type_error();
    }
  }
  
  
  return ret;
}

struct type *get_type_exp(struct exp* exp){
  
  switch (exp->kind) {
    case EXP_UNIT:
      return type_unit();
      break;
    case EXP_TRUE:
    case EXP_FALSE:
      return type_bool();
      break;
    case EXP_I32:
      return type_i32();
      break;
    case EXP_U8:
      return type_u8();
      break;
    case EXP_STR:
      return type_ref(type_u8());
      break;
    case EXP_ID:
      return nearest_type(exp->id);
      //return type_error();
      // Get type of id from nearest scope
      // If id not defined, error
      //symbol_print(exp->id);
      break;
    case EXP_ENUM:
      return type_error();
      /* 
        Don't do?
      print_head("enum");
      print_head("enum-ctor");
      symbol_print(exp->lit_enum.eid);
      symbol_print(exp->lit_enum.cid);
      print_rparen();
      if (exp->lit_enum.exps) {
            print_head("exprs");
            g_list_foreach(exp->lit_enum.exps, (GFunc)exp_print, NULL);
            print_rparen();
      }
      print_rparen();
      */
      break;
    case EXP_STRUCT:
      return type_error();
      /*
      print_head("struct");
      symbol_print(exp->lit_struct.id);
      print_head("field-inits");
      g_list_foreach(exp->lit_struct.fields, (GFunc)pair_print, NULL);
      print_rparen();
      print_rparen();
      */
      break;
    case EXP_ARRAY:
      return type_error();
    /*
      print_head("arr");
      print_head("exprs");
      g_list_foreach(exp->lit_array.exps, (GFunc)exp_print, NULL);
      print_rparen();
      print_rparen();
      */
      break;
    case EXP_LOOKUP:
      return type_error();
    /*
      print_head("field-lookup");
      exp_print(exp->lookup.exp);
      symbol_print(exp->lookup.id);
      print_rparen();
      */
      break;
    case EXP_INDEX:
      return type_error();
    /*
      print_head("arr-index");
      exp_print(exp->index.exp);
      exp_print(exp->index.idx);
      print_rparen();
      */
      break;
    case EXP_FN_CALL:
      return type_error();
    /*
      print_head("fn-call");
      symbol_print(exp->fn_call.id);
      if (exp->fn_call.exps) {
            print_head("exprs");
            g_list_foreach(exp->fn_call.exps, (GFunc)exp_print, NULL);
            print_rparen();
      }
      print_rparen();
      */
      break;
    case EXP_BOX_NEW:
      exp->exp->type = get_type_exp(exp->exp);
    
      if (exp->exp->type->kind != TYPE_ERROR)
        return type_box(exp->exp->type);
      
      return type_error();
      
      break;
    case EXP_MATCH:
      /*
        Don't have to do?
      print_head("match");
      exp_print(exp->match.exp);
      print_head("match-arms");
      g_list_foreach(exp->match.arms, (GFunc)pair_print, NULL);
      print_rparen();
      print_rparen();
      */
      break;
    case EXP_IF:
      // type-t if true and false have type-t and cond has type-bool
      // type-unit if true has type-unit, false is NULL, and cond has type-bool
      exp->if_else.block_true->type = get_type_exp(exp->if_else.block_true);
      exp->if_else.cond->type = get_type_exp(exp->if_else.cond);
      
      if  (exp->if_else.block_false){
        exp->if_else.block_false->type = get_type_exp(exp->if_else.block_false);
        
        if (exp->if_else.block_true->type->kind == 
            exp->if_else.block_false->type->kind && 
            exp->if_else.cond->type->kind == TYPE_BOOL)
          return exp->if_else.block_true->type;
        
      }
      else{
        if (exp->if_else.block_true->type->kind == 
            TYPE_UNIT && 
            exp->if_else.cond->type->kind == TYPE_BOOL)
          return exp->if_else.block_true->type;
      }
      
      return type_error();
      break;
    case EXP_WHILE:
      // type-unit if cond is bool and block is type-unit
      exp->loop_while.cond->type = get_type_exp(exp->loop_while.cond);
      exp->loop_while.block->type = get_type_exp(exp->loop_while.block);
      if (exp->loop_while.cond->type->kind == TYPE_BOOL &&
          exp->loop_while.block->type->kind == TYPE_UNIT)
        return type_unit();
    
      return type_error();
      break;
    case EXP_LOOP:
      exp->exp->type = get_type_block(exp->exp, type_unit());
      // type-unit if block is type-unit
      if (exp->exp->type->kind == TYPE_UNIT)
        return type_unit();
      
      return type_error();
      break;
    case EXP_BLOCK:
      exp->type = get_type_block(exp, type_unit());
      return exp->type;

      break;
    case EXP_UNARY:
      exp->unary.exp->type = get_type_exp(exp->unary.exp);
      
      // NOT
      // type-bool if e1 is bool
      if (!strcmp(exp->unary.op, "!"))
        if (exp->unary.exp->type->kind == TYPE_BOOL)
          return type_bool();
      
      // Neg
      // type-i32 is e1 is i32
      if (!strcmp(exp->unary.op, "-"))
        if (exp->unary.exp->type->kind == TYPE_I32)
          return type_i32();
      
      return type_error();
    
      //if (!strcmp(op, "&")) return mut? "addr-of-mut" : "addr-of";
      //if (!strcmp(op, "!")) return "not";
      //if (!strcmp(op, "-")) return unary? "neg" : "sub";
      //if (!strcmp(op, "*")) return unary? "deref" : "mul";
      break;
    case EXP_BINARY:
      // TODO: Maybe move to own function?
      exp->binary.left->type = get_type_exp(exp->binary.left);
      exp->binary.right->type = get_type_exp(exp->binary.right);

      // Bool
      //   type-bool if e1 and e2 are bool
      //   type-error else
      if (!strcmp(exp->binary.op, "&&") || !strcmp(exp->binary.op, "||")){
        if (exp->binary.left->type->kind == TYPE_BOOL && exp->binary.right->type->kind == TYPE_BOOL)
          return type_bool();
      }
      
      // i32 math
      //   type-i32 if e1 and e2 are type-i32
      //   type-error else
      if (!strcmp(exp->binary.op, "+") || !strcmp(exp->binary.op, "-") ||
          !strcmp(exp->binary.op, "*") || !strcmp(exp->binary.op, "/") ||
          !strcmp(exp->binary.op, "%")                                    ){
        if (exp->binary.left->type->kind == TYPE_I32 && exp->binary.right->type->kind == TYPE_I32)
          return type_i32();
      }
      // Comparison ops
      //   type-bool if e1 and e2 are type-i32
      //   type-error else
      if (!strcmp(exp->binary.op, "<") || !strcmp(exp->binary.op, "<=") ||
          !strcmp(exp->binary.op, ">") || !strcmp(exp->binary.op, ">=")    ){
        if (exp->binary.left->type->kind == TYPE_I32 && exp->binary.right->type->kind == TYPE_I32)
          return type_bool();
      }
      // Equality ops
      //    type-bool if e1  and e2 are same type
      //    type-error else
      if (!strcmp(exp->binary.op, "==") || !strcmp(exp->binary.op, "!=")){
        if (exp->binary.left->type->kind == exp->binary.right->type->kind)
          return type_bool();
      }
    
      return type_error();
      break;
  }
  
  return type_error();
}

struct type *nearest_type(Symbol s){
  struct env *curr;
  struct type *temp;
  //Symbol sym = symbol_type(strdup(name));
  
  // Get first instance of the symbol that isn't an error
  // If exits loop, return error
  for (GList *i = envs; i; i = i->next){
    
    curr = i->data;
    temp = env_lookup(curr, s);
    if (temp && temp->kind != TYPE_ERROR)
      return temp;
    
  }
  
  return type_error();
}

GList* add_environment(){
  envs = g_list_prepend(envs, env_new());

  return envs; 
}

void delete_environment(){
  envs = g_list_remove(envs, g_list_first(envs)->data); 
}

struct env* get_first_environment(){
  return g_list_first(envs)->data;
}

