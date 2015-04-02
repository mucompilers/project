#include "ast.h"
#include <stdlib.h>
#include <stdio.h>
#include <glib.h>
#include "parser.h"

GNode * ast_node_new(int kind, void * lexeme){
	struct ast * ast = malloc(sizeof * ast);
	ast->kind = kind;
	switch(kind){
		case ID:
		case LITSTR:
			ast->str = (char *)lexeme;
			break;
		case LITDEC: 
		case TRU:
		case FALS:
			ast->num = (int)lexeme;
			break;
	 	case LITCHAR: 
			ast->c = (char)lexeme;
			break;
		default:
			break;
	}

	return g_node_new((void *)ast);
}



void node_print(GNode *node) {

      struct ast * my_ast = (struct ast*)( node->data );
      int kind = my_ast->kind;		


      /*===PRINT CORRECT TABS===*/
      guint ntabs = g_node_depth(node);
      int i;
      printf("\n");
      for(i=1; i<ntabs;i++) printf("    ");      
	
      

            printf("(");
            switch (kind) {
		  case CRATE:
			printf("crate");
			break;
		  /*===ITEMS===*/
		  case ITEM:
			break;
		  case ITEMS:
			printf("items");
			break;
		
                  case FNDEF_ITEM:
                        printf("fn-def");
                        break;
		  case FNDEFPARAMS_ITEM:
			printf("fn-params");
			break;	
		  case FNDEFPARAM_ITEM:
			printf("fn-param");
			break;
		  case FNDEFRET_ITEM:
			//Do nothing
			break;
		  case FNDEFBLOCK_ITEM:
			//do nothing
			break;	

		  case STRUCTDEF_ITEM:
			printf("struct-def");
			break;
		  case STRUCTDEFFIELDS_ITEM:
			printf("field-defs");
			break;
		  case STRUCTDEFFIELD_ITEM:
			printf("field-def");
                        break;
		  case ENUMDEF_ITEM:
			printf("enum-def");
			break;
		  case ENUMDEFFIELDS_ITEM:
			printf("enum-ctor-defs");
			break;		  
		  case ENUMDEFFIELD_ITEM:
			printf("enum-ctor-def");
			break;
		  case ENUMDEFFIELDTYPES_ITEM:
			printf("enum-ctor-params");
			break;

		  /*===PATTERNS===*/
		  case PAT:
			//do nothing
			break;
		  case UNIT_PAT:
			printf("pat-unit");
			break;
		  case LIT_PAT:
		  	printf("pat-lit");
			break;
		  case ENUM_PAT:
			printf("pat-enum");
			break;
		  case ENUMFIELDS_PAT:
			printf("pat-enum-ctor-params");
			break;	
		
		  case STRUCT_PAT:
			printf("pat-struct");
                        break;
		  case STRUCTFIELDS_PAT:	
			printf("pat-fields");
                        break;	
		  case STRUCTFIELD_PAT:
			printf("pat-field");
			break;
		  case ID_PAT:
			printf("pat-id");
			break;
		  case REF_PAT:
			printf("pat-ref-id");
			break;
		  case REFMUT_PAT:
			printf("pat-ref-mut-id");
			break;
		  case MUT_PAT:
			printf("pat-mut-id");
			break;
		  case ARRAY_PAT:
			printf("pat-arr");		 	
			break;
		  case ARRAYELMTS_PAT:
			printf("pat-arr-elems");
			break;
		  case DEREF_PAT:
			printf("pat-deref");
		  	break;
/*parser.y*/	  case UNDERSCORE:
			printf("pat-wild");
			break;
/*parser.y*/  	  case ID:
                        printf("id\n");
			for(i=1; i<ntabs+1;i++) printf("    ");
			printf("(%s)", my_ast->str);
                        break;
		

		  /*===TYPES===*/
/*parser.y*/	  case I32:
			printf("type-i32");
			break;	
/*parser.y*/	  case U8:
			printf("type-u8");
			break;
/*parser.y*/	  case BOOL:
			printf("type-bool");
			break;
/*parser.y*/	  case UNIT_TYPE:
			printf("type-unit");
			break;
		  case BOX_TYPE:
			printf("type-box");
			break;
		  case ARR_TYPE:
			printf("type-arr");
			break;
		  case ARRSIZE_TYPE:
			printf("type-arr");
		  	break;
		  case ADDR_TYPE:
			printf("type-ref");
			break;
		  case ADDRMUT_TYPE:
			printf("type-ref-mut");
			break;
		  


		  /*===EXPRESSIONS===*/
		  case EXP:
			printf("expr");
			break;

		  case IF_EXP:
			printf("if");
			break;
		  case IFEVAL_EXP:
			//do nothing
			break;
		  case IFBLOCK_EXP:
			//do nothing
			break;
		  case ELSEBLOCK_EXP:
			//do nothing
			break;


	 	  case LOOP_EXP:
			printf("loop");
		  	break;
		  case LOOPBLOCK:
			//do nothing
			break;

		  case WHILE_EXP:
			printf("while");
			break;
		  case WHILEEVAL_EXP:
			//do nothing
			break;
		  case WHILEBLOCK_EXP:
			//do nothing
			break;


		  case MATCH_EXP:
			printf("match");
			break;
		  case MATCHEXP_EXP:
			//Do nothing
			break;
		  case MATCHARMS_EXP:
			printf("match-arms");
			break;
		  case MATCHARM_EXP:
			printf("match-arm");
		  	break;
		  case PATOR_EXP:
			printf("pats");
			break;
		  case MATCHBLOCK_EXP:
			//do nothing
			break;
		  case BOXNEW_EXP:
			printf("box-new");
			break;		  
		  case BOXNEWEXP_EXP:
			printf("exprs");
			break;
		  case ARRIDX_EXP:
			printf("arr-index");
			break;
		  case ARRIDXEXP_EXP:
			//do nothing
			break;
		  case FIELDLUP_EXP:
			printf("field-lookup");
			break;
		  case FIELD_EXP:
			//do nothing
			break;
/*parser.y*/	  case ASSIGNMENT:
			printf("assign");
			break;
/*parser.y*/	  case PLUS_ASSIGN:
			printf("assign-add");
			break;
/*parser.y*/	  case SUB_ASSIGN:
			printf("assign-sub");
			break;
/*parser.y*/	  case DIV_ASSIGN:
			printf("assign-div");
			break;
/*parser.y*/	  case MUL_ASSIGN:
			printf("assign-mul");
			break;
/*parser.y*/	  case REM_ASSIGN:
			printf("assign-rem");	
			break;
			

		  case MUL_EXP:
			printf("mul");
			break;
		  case ADD_EXP:
			printf("add");
			break;
		  case DIV_EXP:
                        printf("div");
                        break;
                  case SUB_EXP:
                        printf("sub");
			break;
		  case REM_EXP:
                        printf("rem");
                        break;
                  case NEG_EXP:
                        printf("neg");
			break;
		  case OR_EXP:
                        printf("or");
                        break;
                  case AND_EXP:
                        printf("and");
                        break;

                  case EQ_EXP:
                        printf("eq");
                        break;
                  case NE_EXP:
                        printf("neq");
                        break;
                  case LT_EXP:
                        printf("lt");
                        break;
                  case GT_EXP:
                        printf("gt");
                        break;
		  case GE_EXP:
                        printf("geq");
                        break;
                  case LE_EXP:
                        printf("leq");
                        break;

                  case NOT_EXP:
                        printf("not");
                        break;

                  case DEREF_EXP:
                        printf("deref");
                        break;
                  case ADDR_EXP:
                        printf("addr-of");
                        break;
                  case ADDRMUT_EXP:
                        printf("addr-of-mut");
                        break;

		  case FNCALL_EXP:
			printf("fn-call");
			break;
		  case FNCALLEMPTY_EXP:
			//printf("");
			break;
		  case FNCALLPARAMS_EXP:
			printf("exprs");
			break;
		  case FNCALLPARAM_EXP:
			//Nothing
			break;

		  /*===STATEMENTS===*/
 		  case STMT:
		  case STMTS:
			//DO NOTHING
			break;		 
 
		  case LET_STMT:
			printf("let");
			break;
		  case LETTYPEASS_STMT:
			printf("let");
			break;		
		  case LETASS_STMT:
			printf("let");
			break;	
		  case LETTYPE_STMT:
			printf("let");
			break;	

		  case RETURN_STMT:
			printf("return");
			break;
		  case RETURNEXP_STMT:
			printf("return");
			break;
                  case BLOCK:
                        printf("block");
                        break;
			
		/*===Literals===*/		 

/*parser.y*/  	  case LITCHAR:
			printf("lit-char");
			break;
/*parser.y*/  	  case LITSTR:
			printf("lit-str");
			break;
/*parser.y*/  	  case LITDEC:
			printf("lit-dec");
			break;
/*parser.y*/      case TRU:
			printf("true");
                        break; 
/*parser.y*/      case FALS:	
			printf("false");
			break;
		  case UNIT_EXP:
			printf("unit");
			break;
		  case ENUMCTOR_LIT:
			printf("enum-ctor");
			break;
		  case ENUM_LIT:
			printf("enum");
		  	break;	   
		  case ENUMFIELDS_LIT:
			printf("exprs");
			break;
		  case ARRAY_LIT:
			printf("arr"); 			
			break;
		  case STRUCT_LIT:
			printf("struct");
			break;
		  case STRUCTFIELDS_LIT:
			printf("field-inits");
			break;	
		  case STRUCTFIELD_LIT:
			printf("field-init");
			break;
		 

		  default:
			printf("\n!!NOT LOCATED IN PRINT FUNCTION!!\n");
			break;
            }
      
     	   
		
	    GNode* child = node->children;	    
	    while(child){
		node_print(child);
		child = child->next;
	    }	
	printf(")");

	
}
