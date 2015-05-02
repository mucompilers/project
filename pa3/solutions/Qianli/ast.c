#include "ast.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>
#include "env.h"

struct ast* ast_unary(int kind, struct ast* ast1){
    struct ast* ast = malloc(sizeof* ast);
    ast->type = NULL;
    ast->kind = kind;
    ast->una_node = ast1;
    return ast;
}
struct ast* ast_binary(int kind, struct ast* ast1, struct ast* ast2){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = kind;
    ast->bin_node1 = ast1;
    ast->bin_node2 = ast2;
    return ast;
}
struct ast* ast_ternary(int kind, struct ast* ast1, struct ast* ast2, struct ast* ast3){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = kind;
    ast->ter_node1 = ast1;
    ast->ter_node2 = ast2;
    ast->ter_node3 = ast3;
    return ast;
}
struct ast* ast_quater(int kind, struct ast* ast1, struct ast* ast2, struct ast* ast3, struct ast* ast4){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = kind;
    ast->quat_node1 = ast1;
    ast->quat_node2 = ast2;
    ast->quat_node3 = ast3;
    ast->quat_node4 = ast4;
    return ast;
}
struct ast* ast_list(int kind, GList * list){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = kind;
    ast->list = list;
    return ast;
}
struct ast* ast_list_gen(int kind, struct ast* ast1){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = kind;
    ast->list = g_list_append(NULL, ast1);
    return ast;
}
struct ast* ast_id(char* str){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = AST_ID;
    ast->id = str;
    return ast;
}
struct ast* ast_lit_dec(int num){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = AST_LIT_DEC;
    ast->lit_dec = num;
    return ast;
}
struct ast* ast_lit_char(char ch){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = AST_LIT_CHAR;
    ast->lit_char = ch;
    return ast;
}
struct ast* ast_lit_str(char* str){
    struct ast* ast = malloc(sizeof * ast);
    ast->type = NULL;
    ast->kind = AST_LIT_STR;
    ast->id = str;
    return ast;
}

static int indent_level = 0;
void indent(){
    int i = 1;
    for(i; i<indent_level; i++)
	printf("    ");
}

void ast_print(struct ast* ast){
    indent_level++;
    printf("\n");
    indent();
    printf("(");
    if(ast==NULL)
	return ;
    else{
	switch(ast->kind){
	    case AST_CRATE:
		printf("crate");type_print(ast);
 		ast_print(ast->una_node);
		break;

	    case AST_ITEMS:
		printf("items");
		if(ast->list!=NULL){
		    g_list_foreach(ast->list, (GFunc)ast_print, NULL);
		}
		break;

	    case AST_FN_DEF1:
		printf("fn-def");type_print(ast);
		ast_print(ast->ter_node1);
		ast_print(ast->ter_node2);
		ast_print(ast->ter_node3);
		break;

	    case AST_FN_DEF2:
		printf("fn-def");type_print(ast);
		ast_print(ast->quat_node1);
		ast_print(ast->quat_node2);
		ast_print(ast->quat_node3);
		ast_print(ast->quat_node4);
		break;

	    case AST_FN_PARAMS:
		printf("fn-params");
		if(ast->list!=NULL)
		    g_list_foreach(ast->list,(GFunc)ast_print,NULL);
		break;

	    case AST_FN_PARAM:
		printf("fn-param");
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_STRUCT_DEF:
		printf("struct-def");printf(":ok!");
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_FIELD_DEFS:
		printf("field-defs");
		if(ast->list!=NULL)
		    g_list_foreach(ast->list,(GFunc)ast_print,NULL);
		break;

	    case AST_FIELD_DEF:
		printf("field-def");
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_BLOCK:
		printf("block");type_print(ast);
		if(ast->list!=NULL){
		    g_list_foreach(ast->list, (GFunc)ast_print, NULL);
		}
		break;

	    case AST_LOOP:
		printf("loop");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_IF1:
		printf("if");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_IF2:
		printf("if");type_print(ast);
		ast_print(ast->ter_node1);
		ast_print(ast->ter_node2);
		ast_print(ast->ter_node3);
		break;

	    case AST_WHILE:
		printf("while");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ADD:
		printf("add");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_SUB:
		printf("sub");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;
	
	    case AST_MUL:
		printf("mul");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_DIV:
		printf("div");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_REM:
		printf("rem");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_AND:
		printf("and");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_OR:
		printf("or");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;
		
	    case AST_NOT:
		printf("not");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_LT:
		printf("lt");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_GT:
		printf("gt");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;
	
	    case AST_LEQ:
		printf("leq");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_GEQ:
		printf("geq");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_EQ:
		printf("eq");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_NEQ:
		printf("neq");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ADDR_OF:
		printf("addr-of");
		ast_print(ast->una_node);
		break;

	    case AST_DEREF:
		printf("deref");
		ast_print(ast->una_node);
		break;

	    case AST_FIELD_LOOKUP:
		printf("field-lookup");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_FN_CALL1:
		printf("fn-call");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_FN_CALL2:
		printf("fn-call");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_BOX_NEW:
		printf("box-new");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_ARR_INDEX:
		printf("arr-index");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ASSIGN:
		printf("assign");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ASSIGN_ADD:
		printf("assign-add");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ASSIGN_SUB:
		printf("assign-sub");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ASSIGN_MUL:
		printf("assign-mul");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ASSIGN_DIV:
		printf("assign-div");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_ASSIGN_REM:
		printf("assign-rem");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_EXPRS:
		printf("exprs");
		if(ast->list!=NULL)
		    g_list_foreach(ast->list,(GFunc)ast_print,NULL);
		break;

	    case AST_ID:
		printf("id");
		if(ast->type!=NULL){
		    //if(ast->type->kind!=NULL){
			//if(ast->type->kind==TYPE_ARRAY){
		            type_print(ast);
			//}
		    //}
		}
		printf("\n    ");
		indent();
		printf("(%s)",ast->id);	
		break;

	    case AST_LIT_DEC:
		printf("lit-dec");type_print(ast);
		break;

  	    case AST_TYPE_REF:
		printf("type-ref");
		ast_print(ast->una_node);
		break;

	    case AST_TYPE_BOX:
		printf("type-box");
		ast_print(ast->una_node);
		break;

	    case AST_LIT_CHAR:
		printf("lit-char");type_print(ast);
		break;

	    case AST_LIT_STR:
		printf("lit-str");type_print(ast);
		break;

	    case AST_TYPE_ARR1:
		printf("arr");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_STRUCT:
		printf("struct");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_FIELD_INITS:
		printf("field-inits");
		if(ast->list!=NULL)
		    g_list_foreach(ast->list,(GFunc)ast_print, NULL);
		break;

	    case AST_FIELD_INIT:
		printf("field-init");
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_TRUE:
		printf("true");type_print(ast);
		break;

	    case AST_FALSE:
		printf("false");type_print(ast);
		break;

	    case AST_UNIT:
		printf("unit");type_print(ast);
		break;

      	    case AST_STMT_EXP:
		printf("stmt-exp");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_RETURN:
		printf("return");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_LET1:
		printf("let");
		ast_print(ast->una_node);
		break;

	    case AST_LET2:
		printf("let");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_LET3:
		printf("let");type_print(ast);
		ast_print(ast->ter_node1);
		ast_print(ast->ter_node2);
		ast_print(ast->ter_node3);
		break;

	    case AST_LET4:
		printf("let");type_print(ast);
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;

	    case AST_PAT_ID:
		printf("pat-id");
		ast_print(ast->una_node);
		break;

	    case AST_PAT_MUT_ID:
		printf("pat-mut-id");type_print(ast);
		ast_print(ast->una_node);
		break;

	    case AST_TYPE_ARR2:
		printf("type-arr");
		ast_print(ast->bin_node1);
		ast_print(ast->bin_node2);
		break;
	    
	    case AST_TYPE_UNIT:
		printf("type-unit");
		break;

	    case AST_TYPE_BOOL:
		printf("type-bool");
		break;
	
	    case AST_TYPE_U8:
		printf("type-u8");
		break;

	    case AST_TYPE_I32:
		printf("type-i32");
		break;

	};
    }
    printf(")");
    indent_level--;
}
