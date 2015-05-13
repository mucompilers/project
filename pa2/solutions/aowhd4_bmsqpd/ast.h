#ifndef AST_H_
#define AST_H_

#include <glib.h>

enum {
      AST_INVALID = 500,
      
      ITEM,
      ITEMS,
      ENUM_DEF,
      ENUM_CTOR_DEFS,
      ENUM_CTOR_DEF,
      ENUM_CTOR_PARAMS,
      STRUCT_DEF,
      STRUCT_ARGS,
      STRUCT_ARG,
      FN_DEF,
      FN_DEF_ARGS,
      FN_DEF_ARG,

      ARRAY_T,
      ARRAY_2_T,
      ARRAY_SIZE_T,
      REF_T,
      MUT_REF_T,
      BOX_T,

      BLOCK,

      STMT,
      STMTS,
      LET_STMT,
      LET_STMT_2,
      LET_STMT_3,
      LET_STMT_4,
      RET_STMT,
      RET_EXP_STMT,

      PAT,
      PATS,
      PRIM_LIT_PAT,
      ID_PAT,
      NAME_BIND_PAT,
      DEREF_PAT,
      ARRAY_PAT,
      ARRAY_ELEM_PAT,
      ENUM_PAT,
      ENUM_ARGS_PAT,
      ENUM_ARG_PAT,
      STRUCT_PAT,
      STRUCT_ARGS_PAT,
      STRUCT_ARG_PAT,
      REF_PAT,
      MUT_PAT,
      REF_MUT_PAT,
      LIT_PAT, 
      REF_MUT_T,


      EXP,
      EXPS,
      PLUS_E,
      MINUS_E,
      MULT_E,
      DIV_E,
      MOD_E,
      NEG_E,
      NOT_E,
      NOTEQ_E,
      EQUAL_E,
      OR_E,
      AND_E,
      LTHAN_E,
      GTHAN_E,
      LEQTHAN_E,
      GEQTHAN_E,
      DEREF_E,
      AMP_E,
      AMP_MUT_E,
      FN_E,
      FN_ARGS_E,
      FN_ARG_E,
      IF_E,
      IF_ELSE_E,
      IF_BLOCK_E,
      ELSE_BLOCK_E,
      LOOP_E,
      LOOP_BLOCK,
      WHILE_E,
      WHILE_ARG_E,
      WHILE_BLOCK,
      NEW_BOX_E,
      NEW_BOX_2_E,
      ARRAY_IND_E,
      ARRAY_IND_E_E,
      LOOKUPS_E,
      LOOKUP_E,
      MATCH_E,
      MATCH_E_E,
      ARMS_E,
      ARM_E,
      MATCH_BLOCK_E,
      MATCH_PAT_E,

      ARRAY_LIT,
      ENUM_LIT,
      ENUM_ARGS_LIT,
      STRUCT_LIT,
      STRUCT_ARGS_LIT,
      STRUCT_ARG_LIT, 

      TYPE_I32,
      TYPE_U8,
      TYPE_UNIT,
      TYPE_BOOL,

};

struct ast {
      int kind;
      union {
            int num;
            char* str;
            char c;
      };
};

GNode * ast_node_new(int, void*);
void ast_print(GNode *);

#endif
