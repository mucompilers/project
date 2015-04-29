#include "ast.h"
#include <stdlib.h>
#include <stdio.h>
#include <glib.h>
#include "parser.h"
#include "type_annotate.h"

GNode * ast_node_new(int kind, void * lexeme){//does primitive type annotating for i32 and bool types
	struct ast * ast = (struct ast *)malloc(sizeof(struct ast));
	
	ast->type = (struct type *)malloc(sizeof(struct type));

	ast->kind = kind;

	//default type marking (may be overriden by in switch() below)
	ast->type->kind = TYPE_INVALID;
	ast->type->length = 0;
	ast->type->id = NULL;
	ast->type->params = NULL;
	ast->type->type = NULL;

	
	switch(kind){
		//types
		case ID:
			ast->type->kind = TYPE_ID;
			ast->type->id = deepcopy( (char*)lexeme );
			ast->str = (char *)lexeme;
			break;
		case BOOL:
			ast->type->kind = TYPE_BOOL;
			break;
		case U8:
			ast->type->kind = TYPE_U8;
			break;
		case I32:
			ast->type->kind = TYPE_I32;
			break;
		case UNIT_TYPE:
			ast->type->kind = TYPE_UNIT;
			break;
		case ARR_TYPE:
			ast->type->kind = TYPE_ARRAY;
			break;
		case SLICE_TYPE:
			ast->type->kind = TYPE_SLICE;
			break;
		case ADDR_TYPE:
			ast->type->kind = TYPE_REF;
			break;
		case ADDRMUT_TYPE:			
			ast->type->kind = TYPE_MUT;
			ast->type->type = (struct type *) malloc(sizeof(struct type));
			ast->type->type->kind = TYPE_REF;
			break;
		case BOX_TYPE:
			ast->type->kind = TYPE_BOX;
			break;
	

		//literals
		case LITSTR:
			ast->str = (char *)lexeme;
			ast->type->kind = TYPE_REF;
			ast->type->type = (struct type *) malloc(sizeof(struct type));
			ast->type->type->kind = TYPE_SLICE;
			ast->type->type->type = (struct type *) malloc(sizeof(struct type));
			ast->type->type->type->kind = TYPE_U8;
			break;
		case LITDEC: 
			ast->type->kind = TYPE_I32;
			ast->num = *((int *)(lexeme));
			break;
		case TRU:
		case FALS:
			ast->num = *((int *)lexeme);
			ast->type->kind = TYPE_BOOL;
			break;
	 	case LITCHAR: 
			ast->c = ((char)lexeme);
			ast->type->kind = TYPE_U8;
			break;
		case UNIT_EXP:
			ast->type->kind = TYPE_UNIT;
			break;
		default:
			break;
	}

	return g_node_new((void *)ast);
}

void type_print(struct type * type) {
	if(!type)return;
	switch(type->kind){
		case TYPE_INVALID:
			printf("invalid"); 
			break;
		case TYPE_OK:
			printf("ok!");
			break;
		case TYPE_ERROR:
			printf("ERROR!");		
			break;		
		case TYPE_UNIT:
			printf("()");
			break;		
		case TYPE_I32:
			printf("i32");
			break;
		case TYPE_U8:
			printf("u8");
			break;
		case TYPE_BOOL:
			printf("bool");
			break;
		case TYPE_ARRAY:
 			printf("[");	
			type_print(type->type);
			printf(";%d]", type->length);
			break;
		case TYPE_REF:
			printf("&");
			type_print(type->type);
			break;
		case TYPE_SLICE:
			printf("[");
			type_print(type->type);
			printf("]");
			break;
 		case TYPE_BOX:
			printf("Box<");
			type_print(type->type);
			printf(">");
			break;
		case TYPE_FN:
			// TODO but i aint gonna
			printf("fn (TODO) -> TODO");
			break;
		case TYPE_ID:
			printf("%s", type->id);
			break;
		case TYPE_MUT:
			printf("mut ");
			type_print(type->type);
			break;
		default:
			printf("HALT:  error in typing engine!\n");
		
	}
}

/*
void print_struct_enum(struct type* t){

	if(t)
		switch(t->kind){
			case TYPE_OK:
				printf("%s", t->id);
				break;
			default:
		}

}*/

void node_print(GNode *node) {
      if(!node)return;
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
			printf("crate:");
			type_print(my_ast->type);
			break;
		  /*===ITEMS===*/
		  case ITEM:
			break;
		  case ITEMS:
			printf("items");
			break;
                  case FNDEF_ITEM:
                        printf("fn-def:");
			if(my_ast->type->kind != TYPE_ERROR){ my_ast->type->kind = TYPE_OK;};
			type_print(my_ast->type);
                        break;
		  case FNDEFPARAMS_ITEM:
			printf("fn-params");
			break;	
		  case FNDEFPARAM_ITEM:
			printf("fn-param");
			break;
		  case FNDEFRET_ITEM:
			//printf("fn-ret");
			//Do nothing
			break;
		  case FNDEFBLOCK_ITEM:
			//do nothing
			break;	

		  case STRUCTDEF_ITEM:
			printf("struct-def:");
			type_print(my_ast->type);
			break;
		  case STRUCTDEFFIELDS_ITEM:
			printf("field-defs");
			break;
		  case STRUCTDEFFIELD_ITEM:
			printf("field-def");
                        break;
		  case ENUMDEF_ITEM:
			printf("enum-def:");
			type_print(my_ast->type);
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
			//type_print(my_ast->type);
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
                        printf("id:");
			type_print(my_ast->type);
			printf("\n");
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
			//type_print(my_ast->type);
			break;
		  case ARR_TYPE:
			printf("type-arr");
			//type_print(my_ast->type);
			break;
		  case ARRSIZE_TYPE:
			printf("type-arr");
		  	break;
		  case ADDR_TYPE:
			printf("type-ref");
			//type_print(my_ast->type);
			break;
		  case ADDRMUT_TYPE:
			printf("type-ref-mut");
			break;
		  


		  /*===EXPRESSIONS===*/
		  case EXP:
			printf("stmt-exp:");
			type_print(my_ast->type);
			break;

		  case IF_EXP:
			printf("if:");
			type_print(my_ast->type);
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
			printf("loop:");
			type_print(my_ast->type);
		  	break;
		  case LOOPBLOCK:
			//do nothing
			break;

		  case WHILE_EXP:
			printf("while:");
			type_print(my_ast->type);
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
			printf("box-new:");
			type_print(my_ast->type);
			break;		  
		  case BOXNEWEXP_EXP:
			printf("exprs");
			//type_print(my_ast->type);
			break;
		  case ARRIDX_EXP:
			printf("arr-index:");
			type_print(my_ast->type);
			break;
		  case ARRIDXEXP_EXP:
			//do nothing
			break;
		  case FIELDLUP_EXP:
			printf("field-lookup:");
			type_print(my_ast->type);
			break;
		  case FIELD_EXP:
			//do nothing
			break;
/*parser.y*/	  case ASSIGNMENT:
			printf("assign:");
			type_print(my_ast->type);
			break;
/*parser.y*/	  case PLUS_ASSIGN:
			printf("assign-add:");
			type_print(my_ast->type);
			break;
/*parser.y*/	  case SUB_ASSIGN:
			printf("assign-sub:");
			type_print(my_ast->type);
			break;
/*parser.y*/	  case DIV_ASSIGN:
			printf("assign-div:");
			type_print(my_ast->type);
			break;
/*parser.y*/	  case MUL_ASSIGN:
			printf("assign-mul:");
			type_print(my_ast->type);
			break;
/*parser.y*/	  case REM_ASSIGN:
			printf("assign-rem:");
			type_print(my_ast->type);	
			break;
			

		  case MUL_EXP:
			printf("mul:");
			type_print(strip_mut(my_ast->type));
			break;
		  case ADD_EXP:
			printf("add:");
			type_print(strip_mut(my_ast->type));
			break;
		  case DIV_EXP:
                        printf("div:");
			type_print(strip_mut(my_ast->type));
                        break;
                  case SUB_EXP:
                        printf("sub:");
			type_print(strip_mut(my_ast->type));
			break;
		  case REM_EXP:
                        printf("rem:");
			type_print(strip_mut(my_ast->type));
                        break;
                  case NEG_EXP:
                        printf("neg:");
			type_print(strip_mut(my_ast->type));
			break;
		  case OR_EXP:
                        printf("or:");
			type_print(strip_mut(my_ast->type));
                        break;
                  case AND_EXP:
                        printf("and:");
			type_print(strip_mut(my_ast->type));
                        break;

                  case EQ_EXP:
                        printf("eq:");
			type_print(strip_mut(my_ast->type));
                        break;
                  case NE_EXP:
                        printf("neq:");
			type_print(strip_mut(my_ast->type));
                        break;
                  case LT_EXP:
                        printf("lt:");
			type_print(strip_mut(my_ast->type));
                        break;
                  case GT_EXP:
                        printf("gt:");
			type_print(strip_mut(my_ast->type));
                        break;
		  case GE_EXP:
                        printf("geq:");
			type_print(strip_mut(my_ast->type));
                        break;
                  case LE_EXP:
                        printf("leq:");
			type_print(strip_mut(my_ast->type));
                        break;

                  case NOT_EXP:
                        printf("not:");
			type_print(strip_mut(my_ast->type));
                        break;

                  case DEREF_EXP:
                        printf("deref:");
			type_print(my_ast->type);
                        break;
                  case ADDR_EXP:
                        printf("addr-of:");
			type_print(my_ast->type);
                        break;
                  case ADDRMUT_EXP:
                        printf("addr-of-mut");
                        break;

		  case FNCALL_EXP:
			printf("fn-call:");
			type_print(my_ast->type);
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
			printf("stmt-exp:");
			type_print(my_ast->type);
			break;		 
 
		  case LET_STMT:
			printf("let:");
			type_print(my_ast->type);
			break;
		  case LETTYPEASS_STMT:
			printf("let:");
			type_print(my_ast->type);
			break;		
		  case LETASS_STMT:
			printf("let:");
			type_print(my_ast->type);
			break;	
		  case LETTYPE_STMT:
			printf("let:");
			type_print(my_ast->type);
			break;	

		  case RETURN_STMT:
			printf("return:");
			type_print(my_ast->type);
			break;
		  case RETURNEXP_STMT:
			printf("return:");
			type_print(my_ast->type);
			break;
                  case BLOCK:
                        printf("block:");
			type_print(my_ast->type);
                        break;
			
		/*===Literals===*/		 

/*parser.y*/  	  case LITCHAR:
			printf("lit-char:");
			type_print(my_ast->type);
			break;
/*parser.y*/  	  case LITSTR:
			printf("lit-str:");
			type_print(my_ast->type);
			break;
/*parser.y*/  	  case LITDEC:
			printf("lit-dec:");
			type_print(my_ast->type);
			break;
/*parser.y*/      case TRU:
			printf("true:");
			type_print(my_ast->type);
                        break; 
/*parser.y*/      case FALS:	
			printf("false:");
			type_print(my_ast->type);
			break;
		  case UNIT_EXP:
			printf("unit:");
			type_print(my_ast->type);
			break;
		  case ENUMCTOR_LIT:
			printf("enum-ctor");
			break;
		  case ENUM_LIT:
			printf("enum:");
			type_print(my_ast->type);
		  	break;	   
		  case ENUMFIELDS_LIT:
			printf("exprs");
			break;
		  case ARRAY_LIT:
			//printf("\n(exprs\n");
			printf("arr:");
			type_print(my_ast->type); 
			//printf(")");			
			break;
		  case STRUCT_LIT:
			printf("struct:");
			type_print(my_ast->type);
			break;
		  case STRUCTFIELDS_LIT:
			printf("field-inits");
			break;	
		  case STRUCTFIELD_LIT:
			printf("field-init");
			//type_print(my_ast->type);
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
