#include "ast.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>


struct ast* ast_biquop_lglg(int kind, struct ast* ast1, GList* list1,struct ast* ast2,GList* list2){
    struct ast* ast = malloc(sizeof *ast);
    ast->kind = kind;
    ast->biquop_a1 = ast1;
    ast->biquop_g1 = list1;
    ast->biquop_a2 = ast2;
    ast->biquop_g2 = list2;
    return ast;
}

struct ast* ast_triop_lgg(int kind, struct ast* ast1, GList* list1, GList* list2){
    struct ast* ast = malloc(sizeof *ast);
    ast->kind = kind;
    ast->if_condition = ast1;
    ast->if_body = list1;
    ast->else_body = list2;
    return ast;
}

struct ast* ast_triop_llg(int kind, struct ast* ast1, struct ast* ast2, GList* list){
    struct ast* ast = malloc(sizeof *ast);
    ast->kind = kind;
    ast->triop_llg1=ast1;
    ast->triop_llg2=ast2;
    ast->triop_llg3=list;
    return ast;
}




struct ast* ast_binop_new(int kind, struct ast* left, struct ast* right) {
      struct ast* ast = malloc(sizeof *ast);
      ast->kind = kind;
      ast->left = left;
      ast->right = right;
      return ast;
}

struct ast* ast_unaop_new(int kind, struct ast* child){
    struct ast* ast = malloc(sizeof *ast);
    ast->child = child;
    ast->kind = kind;
    return ast;
}

struct ast* ast_binop_lg(int kind, struct ast* child, GList * list){
    struct ast* ast = malloc(sizeof *ast);
    ast->kind = kind;
    ast->while_left = child;
    ast->while_right= list;
    return ast;
}

struct ast* ast_unaop_g(int kind, GList * list){
    struct ast* ast=malloc(sizeof *ast);
    ast->kind = kind;
    ast->loop = list;
    return ast;
}

struct ast* ast_binop_gg(int kind, GList* list1, GList* list2){
    struct ast* ast=malloc(sizeof * ast);
    ast->kind = kind;
    ast->if_body = list1;
    ast->else_body = list2;
    return ast;
}


struct ast* ast_pat_wild_new(){
    struct ast* ast=malloc(sizeof * ast);
    ast->kind = AST_PAT_WILD;
    return ast;
}


struct ast* ast_lit_dec_new(int num) {
      struct ast* ast = malloc(sizeof *ast);
      ast->kind = AST_LIT_DEC;
      ast->lit_dec_child = num;
      return ast;
}

struct ast* ast_id_new(char* name){
    struct ast* ast = malloc(sizeof *ast);
    ast->kind = AST_ID;
    ast->id_child = name;
    return ast;
}

struct ast* ast_block_new(){
    struct ast* ast = malloc(sizeof * ast);
    ast->kind = AST_BLOCK;
    return ast;
}
struct ast* ast_while_new(struct ast* child, GList* list){
    struct ast* ast = malloc(sizeof *ast);
    ast->kind = AST_WHILE;
    ast->while_left = child;
    ast->while_right = list;
    return ast;
}

struct ast* ast_loop_new(GList * list){
    struct ast* ast = malloc(sizeof * ast);
    ast->kind = AST_LOOP;
    ast->loop = list;
    return ast;
}

struct ast* ast_if_new1(struct ast* child, GList *list){
    struct ast* ast= malloc(sizeof *ast);
    ast->kind = AST_IF1;
    ast->if_condition = child;
    ast->if_body = list;
    ast->else_body = NULL;
    return ast;
}

struct ast* ast_if_new2(struct ast* child, GList *list1, GList *list2){
    struct ast* ast=malloc(sizeof *ast);
    ast->kind = AST_IF2;
    ast->if_condition = child;
    ast->if_body = list1;
    ast->else_body = list2;
    return ast;
}

struct ast* ast_bool_new(int num){
    struct ast * ast = malloc(sizeof *ast);
    if(num==1) ast->kind = AST_TRUE;
    else ast->kind = AST_FALSE;
    return ast;
}

struct ast* ast_lit_char_new(char ch){
    struct ast * ast = malloc(sizeof * ast);
    ast->kind = AST_LIT_CHAR;
    ast->lit_char_child = ch;
    return ast;
}

struct ast* ast_lit_str_new(char * ch){
    struct ast * ast=malloc(sizeof * ast);
    ast->kind = AST_LIT_STR;
    ast->id_child = ch;
    return ast;
}

struct ast* ast_unit_new(){
    struct ast * ast=malloc(sizeof * ast);
    ast->kind = AST_UNIT;
    return ast;
}


struct ast* ast_enum_ctor_params(struct ast * child, GList * list){
    struct ast* ast=malloc(sizeof * ast);
    ast->kind = AST_ENUM;
    ast->while_left = child;
    ast->while_right = list;
    return ast;
}

struct ast* ast_arr_new(GList *list){
    struct ast* ast=malloc(sizeof *ast);
    ast->kind = AST_ARR;
    ast->loop = list;
    return ast;
}

struct ast* ast_pat_elems_new(GList *list){
    struct ast* ast=malloc(sizeof *ast);
    ast->kind = AST_PAT_ELEM;
    ast->loop = list;
    return ast;
}

struct ast* ast_pat_enum_new(struct ast* child,GList* list){
    struct ast* ast =malloc(sizeof *ast);
    ast->kind=AST_PAT_ENUM;
    ast->while_left = child;
    ast->while_right= list;
    return ast;
}

struct ast* ast_empty_new(int kind){
    struct ast* ast = malloc(sizeof * ast);
    ast->kind = kind;
    return ast;
}

struct ast* ast_triop_new(int kind, struct ast* ast1,struct ast* ast2, struct ast * ast3){
    struct ast* ast=malloc(sizeof * ast);
    ast->kind = kind;
    ast->triop_1=ast1;
    ast->triop_2=ast2;
    ast->triop_3=ast3;
    return ast;
}

 
extern int environment = 0;
void indent(int num){
    int i=environment-num;
    while(i>=0&&num!=0){
	printf(")");
	i--;
    }
    environment = num;
    if(num!=0){
	printf("\n");
    }
    while(num>0){
	printf("    ");
	num--;
    }
    printf("(");
}

void ast_print1(struct ast* ast, int pattern){
    ast_print(ast,pattern);
    while(environment>=0){
	printf(")");
	environment--;
    }
}


void ast_print(struct ast* ast, int pattern) {
    if(!ast) return;
	
    switch(ast->kind) {
	    case AST_LIT_DEC:
	  	indent(pattern);
		printf("lit-dec");
		break;

	    case AST_ID:
		indent(pattern);
		printf("id");
		indent(pattern+1);
		printf("%s",ast->id_child);
		break;

	    case AST_ADD:
		indent(pattern);
		printf("add");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_SUB:
		indent(pattern);
	    	printf("sub");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_MUL:
		indent(pattern);
	    	printf("mul ");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_DIV:
		indent(pattern);
	    	printf("div ");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_REM:
		indent(pattern);
	    	printf("rem ");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_NEG:
		indent(pattern);
		printf("neg");
		ast_print(ast->child, pattern+1);
		break;
	    case AST_AND:
		indent(pattern);
		printf("and");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_OR:
		indent(pattern);
		printf("or");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_NOT:
		indent(pattern);
		printf("not");
		ast_print(ast->child, pattern+1);
		break;
	    case AST_LT:
		indent(pattern);
		printf("lt");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_GT:
		indent(pattern);
		printf("gt");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_LEQ:
		indent(pattern);
		printf("leq");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_GEQ:
		indent(pattern);
		printf("geq");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_EQ:
		indent(pattern);
		printf("eq");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_NEQ:
		indent(pattern);
		printf("neq");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_ADDR_OF:
		indent(pattern);
		printf("addr-of");
		ast_print(ast->child, pattern+1);
		break;
	    case AST_ADDR_OF_MUT:
		indent(pattern);
		printf("addr-of-mut");
		ast_print(ast->child, pattern+1);
		break;
	    case AST_DEREF:
		indent(pattern);
		printf("deref");
		ast_print(ast->child, pattern+1);
		break;
	    case AST_FIELD_LOOKUP:
		indent(pattern);
		printf("field-lookup");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_ARR_INDEX:
		indent(pattern);
		printf("arr-index");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_ASSIGN:
		indent(pattern);
		printf("assign");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right,pattern+1);
		break;
	    case AST_ASSIGN_ADD:
		indent(pattern);
		printf("assign-add");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_ASSIGN_SUB:
		indent(pattern);
		printf("assign-sub");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right, pattern+1);
		break;
	    case AST_ASSIGN_MUL:
		indent(pattern);
		printf("assign-mul");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right,pattern+1);
		break;
	    case AST_ASSIGN_DIV:
		indent(pattern);
		printf("assign-div");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right,pattern+1);
		break;
	    case AST_ASSIGN_REM:
		indent(pattern);
		printf("assign-rem");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right,pattern+1);
		break;

	    case AST_WHILE:
		indent(pattern);
		printf("while");
		ast_print(ast->while_left, pattern+1);
		indent(pattern+1);
		printf("block");
		if(ast->while_right!=NULL)
		    g_list_foreach(ast->while_right, (GFunc)ast_print, pattern+2);
		break;
		
	    case AST_LOOP:
		indent(pattern);
		printf("loop");
		indent(pattern+1);
		printf("block");
		if(ast->loop!=NULL)
		    g_list_foreach(ast->loop, (GFunc)ast_print, pattern+2);
		break;

	    case AST_IF1:
		indent(pattern);
		printf("if");
		ast_print(ast->if_condition,pattern+1);
		indent(pattern+1);
		printf("block");
		if(ast->if_body!=NULL)
		    g_list_foreach(ast->if_body, (GFunc)ast_print, pattern+2);
		break;

	   case AST_IF2:
		indent(pattern);
		printf("if");
		ast_print(ast->if_condition,pattern+1);
		indent(pattern+1);
		printf("block");
		if(ast->if_body!=NULL)
		    g_list_foreach(ast->if_body, (GFunc)ast_print, pattern+2);
		indent(pattern+1);
		printf("block");
		if(ast->else_body!=NULL)
		    g_list_foreach(ast->else_body, (GFunc)ast_print, pattern+2);
		break;

	    case AST_MATCH:
		indent(pattern);
		printf("match");
		ast_print(ast->while_left ,pattern+1);
		indent(pattern+1);
		printf("match-arms");
		g_list_foreach(ast->while_right, (GFunc)ast_print, pattern+2);
		break;
		
	    case AST_MATCH_ARM:
		indent(pattern);
		printf("match-arm");
		indent(pattern+1);
		printf("pats");
	 	g_list_foreach(ast->if_body,(GFunc)ast_print, pattern+2);
		indent(pattern+1);
		printf("block");
		if(ast->else_body!=NULL) 
		    g_list_foreach(ast->else_body,(GFunc)ast_print,pattern+2);
		break;

	    case AST_PAT_LIT:
		indent(pattern);
		printf("pat-lit");
		ast_print(ast->child,pattern+1);
		break;

	    case AST_PAT_ID:
		indent(pattern);
		printf("pat-id");
		ast_print(ast->child,pattern+1);
		break;

	    case AST_PAT_REF_ID:
		indent(pattern);
		printf("pat-ref-id");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_PAT_REF_MUT_ID:
		indent(pattern);
		printf("pat-ref-mut-id");
		ast_print(ast->child, pattern+1);
		break;

  	    case AST_PAT_MUT_ID:
		indent(pattern);
		printf("pat-mut-id");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_PAT_DEREF:
		indent(pattern);
		printf("pat-deref");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_PAT_ARR:
		indent(pattern);
		printf("pat-arr");
		ast_print(ast->child,pattern+1);
		break;

	    case AST_PAT_ELEM:
		indent(pattern);
		printf("pat-elem");
		g_list_foreach(ast->loop, (GFunc)ast_print, pattern+1);
		break;

	    case AST_PAT_ENUM:
		indent(pattern);
		printf("pat-enum");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_PAT_ENUM_CTOR_PARAMS:
		indent(pattern);
		printf("pat-enum");
		ast_print(ast->while_left, pattern+1);
	        indent(pattern+1);
		printf("pat-enum-ctor-params");
		g_list_foreach(ast->while_right, (GFunc)ast_print, pattern+2);
		break;

	    case AST_PAT_STRUCT:
		indent(pattern);
		printf("pat-struct");
		ast_print(ast->while_left,pattern+1);
		indent(pattern+1);
		printf("pat-fields");
		g_list_foreach(ast->while_right, (GFunc)ast_print, pattern+2);
		break;

	    case AST_FIELD_INIT:
		indent(pattern);
		printf("field-init");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right,pattern+1);
		break;

	    case AST_PAT_WILD:
		indent(pattern);
		printf("pat-wild");
		break;

	    case AST_ENUM:
		indent(pattern);
		printf("enum");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_ENUM_CTOR:
		indent(pattern);
		printf("enum-ctor");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right,pattern+1);
		break;

	    case AST_ENUM_CTOR_PARAMS:
		indent(pattern);
		printf("enum");
		ast_print(ast->while_left,pattern+1);
		indent(pattern+1);
		printf("exprs");
		g_list_foreach(ast->while_right, (GFunc)ast_print, pattern+2);
		break;

	    case AST_STRUCT:
		indent(pattern);
		printf("struct");
		ast_print(ast->while_left, pattern+1);
		indent(pattern+1);
		printf("field_inits");
		g_list_foreach(ast->while_right,(GFunc)ast_print, pattern+2);
		break;

      	    case AST_ARR:
		indent(pattern);
		printf("arr");
		if(ast->child!=NULL) 
		    ast_print(ast->child, pattern+1);
		break;

	    case AST_EXPRS:
		indent(pattern);
		printf("exprs");
		if(ast->loop!=NULL)
		    g_list_foreach(ast->loop,(GFunc)ast_print, pattern+1);
		break;




	    case AST_LIT_CHAR:
		indent(pattern);
		printf("lit-char");
		break;

	    case AST_LIT_STR:
		indent(pattern);
		printf("lit-str");
		break;
		
   	    case AST_TRUE:
		indent(pattern);
		printf("true");
		break;

	    case AST_FALSE:
		indent(pattern);
		printf("false");
		break;
		
   	    case AST_BLOCK:
		indent(pattern);
		printf("block");
		break;
		
	    case AST_PAT_FIELD:
		indent(pattern);
		printf("pat-field");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right,pattern+1);
		break;

	    case AST_FN_CALL:
		indent(pattern);
		printf("fn-call");
		ast_print(ast->left,pattern+1);
		if(ast->right!=NULL)
		    ast_print(ast->right,pattern+1);
		
		break;
	    case AST_FN_CALL1:
		indent(pattern);
		printf("fn-call");
		ast_print(ast->left,pattern+1);
		indent(pattern+1);
		printf("");
		break;
	    case AST_BOX_NEW:
		indent(pattern);
		printf("box-new");
		if(ast->child!=NULL)
		    ast_print(ast->child, pattern+1);
		break;

	    case AST_LET1:
		indent(pattern);
		printf("let");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_LET2:
		indent(pattern);
		printf("let");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right, pattern+1);
		break;

	    case AST_LET3:
		indent(pattern);
		printf("let");
		ast_print(ast->triop_1,pattern+1);
		ast_print(ast->triop_2,pattern+1);
		ast_print(ast->triop_3,pattern+1);
		break;

	    case AST_LET4:
		indent(pattern);
		printf("let");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right,pattern+1);
		break;
	    
	    case AST_RETURN1:
		 indent(pattern);
		 printf("return");
		 break;

	    case AST_RETURN2:
		 indent(pattern);
		 printf("return");
		 ast_print(ast->child, pattern+1);
		 break;

	    case AST_TYPE_ARR1:
		indent(pattern);
		printf("type-arr");
		ast_print(ast->left, pattern+1);
		ast_print(ast->right,pattern+1);
		break;

	    case AST_TYPE_ARR2:
		indent(pattern);
		printf("type-arr");
		ast_print(ast->child, pattern+1);
		break;

  	    case AST_TYPE_REF:
		indent(pattern);
		printf("type-ref");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_TYPE_MUT_REF:
		indent(pattern);
		printf("type-mut-ref");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_TYPE_BOX:
		indent(pattern);
		printf("type-box");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_TYPE_UNIT:
		indent(pattern);
		printf("type-unit");
		break;

	    case AST_TYPE_BOOL:
		indent(pattern);
		printf("type-bool");
		break;

	    case AST_TYPE_U8:
		indent(pattern);
		printf("type-u8");
		break;

	    case AST_TYPE_I32:
		indent(pattern);
		printf("type-i32");
		break;

	    case AST_EMPTY:
		indent(pattern);
		printf("unit");
		break;

	    case AST_CRATE:
		indent(pattern);
		printf("crate");
		indent(pattern+1);
		printf("items");
		g_list_foreach(ast->loop,(GFunc)ast_print, pattern+2);
		break;

	    case AST_FN_DEF1:
		indent(pattern);
		printf("fn-def");
		ast_print(ast->biquop_a1,pattern+1);
		indent(pattern+1);
		printf("fn-params");
		if(ast->biquop_g1!=NULL)
		    g_list_foreach(ast->biquop_g1,(GFunc)ast_print,pattern+2);
		ast_print(ast->biquop_a2,pattern+1);
		indent(pattern+1);
		printf("block");
		if(ast->biquop_g2!=NULL)
		    g_list_foreach(ast->biquop_g2,(GFunc)ast_print,pattern+2);
		break;

	    case AST_FN_DEF2:
		indent(pattern);
		printf("fn-def");
		ast_print(ast->if_condition, pattern+1);
		indent(pattern+1);
		printf("fn-params");
		if(ast->if_body!=NULL)
		    g_list_foreach(ast->if_body,(GFunc)ast_print,pattern+2);
		indent(pattern+1);
		printf("block");
		if(ast->else_body!=NULL)
		    g_list_foreach(ast->else_body,(GFunc)ast_print,pattern+2);
		break;	
	    case AST_FN_DEF3:
		indent(pattern);
		printf("fn-def");
		ast_print(ast->triop_llg1,pattern+1);
		ast_print(ast->triop_llg2,pattern+1);
		indent(pattern+1);
		printf("block");
		if(ast->triop_llg3!=NULL)
		    g_list_foreach(ast->triop_llg3,(GFunc)ast_print,pattern+2);
		break;

	    case AST_FN_DEF4:
		indent(pattern);
		printf("fn-def");
		ast_print(ast->while_left, pattern+1);
		indent(pattern+1);
		printf("block");
		if(ast->while_right!=NULL)
		    g_list_foreach(ast->while_right,(GFunc)ast_print,pattern+2);
		break;

	    case AST_FN_PARAM:
		indent(pattern);
		printf("fn-param");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right,pattern+1);
		break;

	    case AST_ENUM_DEF:
		indent(pattern);
		printf("enum-def");
		ast_print(ast->while_left,pattern+1);
		indent(pattern+1);
		printf("enum-ctor-defs");
		if(ast->while_right!=NULL)
		    g_list_foreach(ast->while_right,(GFunc)ast_print,pattern+2);
		break;

	    case AST_ENUM_CTOR_DEF1:
		indent(pattern);
		printf("enmu-ctor-def");
		ast_print(ast->child, pattern+1);
		break;

	    case AST_ENUM_CTOR_DEF2:
		indent(pattern);
		printf("enum-ctor-def");
		ast_print(ast->while_left, pattern+1);
		indent(pattern+1);
		printf("enum-ctor-params");
		if(ast->while_right!=NULL)
		    g_list_foreach(ast->while_right,(GFunc)ast_print,pattern+2);
		break;

	    case AST_STRUCT_DEF:
		indent(pattern);
		printf("struct-def");
		ast_print(ast->while_left,pattern+1);
		indent(pattern+1);
		printf("field-defs");
		if(ast->while_right!=NULL)
		    g_list_foreach(ast->while_right,(GFunc)ast_print,pattern+2);
		break;

	    case AST_FIELD_DEF:
		indent(pattern);
		printf("field-def");
		ast_print(ast->left,pattern+1);
		ast_print(ast->right, pattern+1);
		break;
		
	    case AST_UNIT:
		indent(pattern);
		printf("unit");
		break;
	    
	    case AST_PAT_UNIT:
		indent(pattern);
		printf("pat-unit");
		break;

	    default: indent(pattern);printf("ERROR");
	}
	//printf(")");
    
    //printf(")");

}
