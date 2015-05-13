#include "ast.h"
#include <stdlib.h>
#include <stdio.h>
#include <glib.h>
#include "parser.h"

GNode * ast_node_new(int kind, void * tokval) {
      struct ast* gast = malloc(sizeof(struct ast));
      gast->kind = kind;
      switch(gast->kind){
            case LITDEC:
            case LITBOOL:
                  gast->num = (int)tokval;
                  break;
            case ID:
            case LITSTR:
                  gast->str = (char *)tokval;
                  break;
            case LITCHAR:
                  gast->c = (char)tokval;
                  break;
            default:
                  break;
      }
      return g_node_new((void*)gast);
}

void ast_print(GNode *gast) {
      struct ast* tree = (struct ast*) (gast->data);

      guint num_tab = g_node_depth(gast);
      int n;
      printf("\n");
      for(n = 1; n < num_tab; n++){
            printf("    ");
      }


      printf("(");
      switch(tree->kind){
            case CRATE:
                  printf("crate");
                  break;

            case ITEMS:
                  printf("items");
                  break;
            case ITEM:
                  break;
            case FN_DEF:
                  printf("fn-def");
                  break;
            case FN_DEF_ARGS:
                  printf("fn-params");
                  break;
            case FN_DEF_ARG:
                  printf("fn-param");
                  break;
            case ENUM_DEF:
                  printf("enum-def");
                  break;
            case ENUM_CTOR_DEFS:
                  printf("enum-ctor-defs");
                  break;
            case ENUM_CTOR_DEF:
                  printf("enum-ctor-def");
                  break;
            case ENUM_CTOR_PARAMS:
                  printf("enum-ctor-params");
                  break;
            case STRUCT_DEF:
                  printf("struct-def");
                  break;
            case STRUCT_ARGS:
                  printf("field-defs");
                  break;
            case STRUCT_ARG:
                  printf("field-def");
                  break;

            case BOX_T:
                  printf("type-box");
                  break;
            case REF_T:
                  printf("type-ref");
                  break;
            case REF_MUT_T:
                  printf("type-ref-mut");
                  break;
            case ARRAY_2_T:
                  printf("type-arr");
                  break;
            case ARRAY_SIZE_T:
                  printf("type-arr");
                  break;

            case BLOCK:
                  printf("block");
                  break;

            case PAT:
                  break;
            case PATS:
                  break;
            case ID:
                  printf("id\n");
                  for (int i = 1; i < num_tab + 1; i++) {
                        printf("    ");
                  }
                  printf("(%s)", tree->str);
                  break;
            case STRUCT_PAT:
                  printf("pat-struct");
                  break;
            case STRUCT_ARGS_PAT:
                  printf("field-inits");
                  break;
            case STRUCT_ARG_PAT:
                  printf("field-init");
                  break;
            case ENUM_PAT:
                  printf("pat-enum");
                  break;
            case ENUM_ARGS_PAT:
                  printf("pat-enum-ctor-params");
                  break;
            case ID_PAT:
                  printf("pat-id");
                  break;
            case ARRAY_PAT:
                  printf("pat-arr");
                  break;
            case ARRAY_ELEM_PAT:
                  printf("pat-arr-elems");
                  break;
            case DEREF_PAT:
                  printf("pat-deref");
                  break;
            case REF_PAT:
                  printf("pat-ref-id");
                  break;
            case MUT_PAT:
                  printf("pat-mut-id");
                  break;
            case REF_MUT_PAT:
                  printf("pat-ref-mut-id");
                  break;
            case UNDERSCORE:
                  printf("pat-wild");
                  break;

            case EXP:
                  printf("expr");
                  break;
            case PLUS_E:
                  printf("add");
                  break;
            case MINUS_E:
                  printf("sub");
                  break;
            case MULT_E:
                  printf("mul");
                  break;
            case DIV_E:
                  printf("div");
                  break;
            case MOD_E:
                  printf("rem");
                  break;
            case NEG_E:
                  printf("neg");
                  break;
            case NOT_E:
                  printf("not");
                  break;
            case NOTEQ_E:
                  printf("neq");
                  break;
            case EQUAL_E:
                  printf("eq");
                  break;
            case OR_E:
                  printf("or");
                  break;
            case AND_E:
                  printf("and");
                  break;
            case LTHAN_E:
                  printf("lt");
                  break;
            case GTHAN_E:
                  printf("gt");
                  break;
            case LEQTHAN:
                  printf("leq");
                  break;
            case GEQTHAN:
                  printf("geq");
                  break;
            case AMP_E:
                  printf("addr-of");
                  break;
            case AMP_MUT_E:
                  printf("addr-of-mut");
                  break;
            case DEREF_E:
                  printf("deref");
                  break;
            case FN_E:
                  printf("fn-call");
                  break;
            case FN_ARGS_E:
                  printf("exps");
                  break;
            case FN_ARG_E:
                  break;
            case IF_E:
                  printf("if");
                  break;
            case IF_ELSE_E:
                  break;
            case IF_BLOCK_E:
                  break;
            case ELSE_BLOCK_E:
                  break;
            case WHILE_E:
                  printf("while");
                  break;
            case WHILE_ARG_E:
                  break;
            case WHILE_BLOCK:
                  break;
            case LOOP_E:
                  printf("loop");
                  break;
            case LOOP_BLOCK:
                  printf("loop");
                  break;
            case NEW_BOX_E:
                  printf("box-new");
                  break;
            case NEW_BOX_2_E:
                  printf("exps");
                  break;
            case ARRAY_IND_E:
                  printf("arr-idx");
                  break;
            case ARRAY_IND_E_E:
                  break;
            case LOOKUPS_E:
                  printf("field-lookup");
                  break;
            case LOOKUP_E:
                  break;
            case MATCH_E:
                  printf("match");
                  break;
            case MATCH_E_E:
                  break;
            case ARMS_E:
                  printf("match-arms");
                  break;
            case ARM_E:
                  printf("match-arm");
                  break;
            case MATCH_PAT_E:
                  printf("pats");
                  break;
            case MATCH_BLOCK_E:
                  break;
            case ASSIGNMENT:
                  printf("assign");
                  break;
            case PLUSEQ:
                  printf("assign-add");
                  break;
            case MINEQ:
                  printf("assign-sub");
                  break;
            case SLASHEQ:
                  printf("assign-div");
                  break;
            case ASTEQ:
                  printf("assign-mul");
                  break;
            case PERCEQ:
                  printf("assign_re");
                  break;



            case LITDEC:
                  printf("lit-dec");
                  break;
            /*case TRU:
                  printf("true");
                  break;
            case FALS:
                  printf("false");
                  break;*/
            case LITCHAR:
                  printf("lit-char");
                  break;
            case LITSTR:
                  printf("lit-str");
                  break;
            case ENUM_LIT:
                  printf("enum-ctor");
                  break;
            case ENUM_ARGS_LIT:
                  printf("exps");
                  break;
            case ARRAY_LIT:
                  printf("arr");
                  break;
            case STRUCT_LIT:
                  printf("struct");
                  break;
            case STRUCT_ARGS_LIT:
                  printf("field-inits");
                  break;
            case STRUCT_ARG_LIT:
                  printf("field-init");
                  break;

            case STMT:
                  break;
            case STMTS:
                  break;
            case LET_STMT:
                  printf("let");
                  break;
            case LET_STMT_2:
                  printf("let");
                  break;
            case LET_STMT_3:
                  printf("let");
                  break;
            case LET_STMT_4:
                  printf("let");
                  break;

            case TYPE_I32:
                  printf("type-i32");
                  break;
            case TYPE_U8:
                  printf("type-u8");
                  break;
            case TYPE_UNIT:
                  printf("type-unit");
                  break;
            case TYPE_BOOL:
                  printf("type-bool");
                  break;

            default:
                  printf("well, this shouldn't have happened");
                  break;
      }

      GNode* child_tree = gast->children;
      while(child_tree){
            ast_print(child_tree);
            child_tree = child_tree->next;
      }
      printf(")");

}
