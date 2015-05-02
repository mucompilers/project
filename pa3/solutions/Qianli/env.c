#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>
#include "env.h"
#include "ast.h"

static int flag = 0;

void type_print(struct ast* ast){
    //assert(ast);
    //if(ast->type==NULL) return;
    //printf(":");
    //single_type_print(ast->type);
}
void single_type_print(struct type* type){
    assert(type);
    switch(type->kind){
	case TYPE_INVALID:
	    printf("ERROR!");
	    break;
	 case TYPE_ERROR:
	    printf("error");
	    break;
	 case TYPE_OK:
	    printf("ok!");
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
	 case TYPE_DIV:
	    printf("div");
	    break;
	 case TYPE_REF:
	    printf("&[");
	    single_type_print(type->type);
	    printf("]");
	    break;
	 case TYPE_MUT:
	    printf("mut ");
	    single_type_print(type->type);
	    break;
	 case TYPE_SLICE:
	    printf("slice");
	    break;
	 case TYPE_ARRAY:
	    printf("[");
	    single_type_print(type->type);
	    printf(";");
	    printf("%d",type->length);
	    printf("]");
	    break;
	 case TYPE_BOX:
	    printf("Box<");
	    single_type_print(type->type);
	    printf(">");
	    break;
	 case TYPE_ID:
	    printf("id");
	    break;
	 case TYPE_STRUCT:
	    printf("%s",type->id);
	    break;
	 case TYPE_FN:
	    printf("ok!");
	    break;
	 case TYPE_ENUM:
	    printf("ok!");
	    break;
	 default:
	    printf("unknown");

    }
}

void ast_calc_type(struct ast* ast, struct env* env){
    flag = 1;
    GList* list = NULL;
    int length = 0;
    struct env* new_env = NULL;
    if(ast==NULL)// || env==NULL)
	return ;
    else{
	switch(ast->kind){
	    case AST_CRATE:
		ast->type = type_ok();
		new_env = env_new();
		for(GList* i=g_list_first(ast->una_node->list);
			i; i=i->next){
		    ast_calc_type((struct ast*)i->data, new_env);
		    if(type_is_invalid(((struct ast*)i->data)->type)){
			    type_destroy(ast->type);
			    ast->type = type_invalid();
		    }
		}
		env_free(new_env);
		break;

	    case AST_FN_DEF1:
		ast_calc_type(ast->ter_node2, env);
		if(judge_is_var(ast->ter_node2)){
	            ast_calc_var(ast->ter_node2, env);
		}
		type_insert("ret",ast->ter_node2->type,env);
		ast_calc_type(ast->ter_node3, env);
		if(!type_is_invalid((ast->ter_node2)->type)
			&& !type_is_invalid((ast->ter_node3)->type)
			&&( type_eq(ast->ter_node2->type,ast->ter_node3->type)
			||  type_eq(ast->ter_node3->type,type_unit()))){
		    //ast->type = type_ok();
		    ast->type = type_fn(NULL, TYPE_FN, ast->ter_node2->type);
		    type_insert(ast->ter_node1->id,ast->type,env);
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_FN_DEF2:
		ast_calc_type(ast->quat_node2, env);
		ast_calc_type(ast->quat_node3, env);
		if(judge_is_var(ast->quat_node3)){
		    ast_calc_var(ast->quat_node3, env);
		}
		type_insert("ret",ast->quat_node3->type, env);
		ast_calc_type(ast->quat_node4, env);
		if(!type_is_invalid((ast->quat_node3)->type)
			&& !type_is_invalid((ast->quat_node4)->type)
			&& (type_eq(ast->quat_node3->type,ast->quat_node4->type)
			||  type_eq(ast->quat_node4->type,type_unit()))){
		    //ast->type = type_ok();
		    ast->type = ast->quat_node2->type;
		    ast->type->type = ast->quat_node3->type;
		    type_insert(ast->quat_node1->id, ast->type, env);
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_FN_PARAMS:
		list = NULL;
		for(GList *i=g_list_first(ast->list);i;i=i->next){
		    ast_calc_type((struct ast*)i->data, env);
		    if(((struct ast*)(i->data))->type){
			list = g_list_append(list,
				((struct ast*)(i->data))->type);
		    }
		}
		if(list){
		    ast->type = type_fn(list, TYPE_FN, NULL);
		    //no return type here
		}
		break;

	    case AST_FN_PARAM:
		ast_calc_type(ast->bin_node2, env);
		if(judge_is_var(ast->bin_node2)){
		    ast_calc_var(ast->bin_node2, env);
		    if(ast->bin_node2->type){
			if(ast->bin_node2->type->kind){
			    type_insert(ast->bin_node1->una_node->id,
			        ast->bin_node2->type,env);
			}
		    }
		}
		else{
		    type_insert(ast->bin_node1->una_node->id,
			ast->bin_node2->type, env);
		}
		ast->type = type_slice(ast->bin_node2->type,
			ast->bin_node1->una_node->id);
	 	break;	

	    case AST_STRUCT_DEF:
		ast_calc_type(ast->bin_node2, env);
		if(ast->bin_node2){
		    ast->type = ast->bin_node2->type;
		    ast->type->id = ast->bin_node1->id;
		    var_insert(ast->bin_node1->id, ast->type, env);
		}
		else{
		    ast->type = type_invalid();
		}
		break;
	    
	    case AST_FIELD_INITS:
	    case AST_FIELD_DEFS:
		list = NULL;
		if(ast->list){
		    for(GList* i=g_list_first(ast->list);i;i=i->next){
			ast_calc_type((struct ast*)(i->data), env);
			if(((struct ast*)(i->data))->type){
			    list = g_list_append(list,
				    (((struct ast*)(i->data))->type));
			}
		    }
		    ast->type = type_item(list, TYPE_STRUCT);
		}
		break;

	    case AST_FIELD_DEF:
	    case AST_FIELD_INIT:
		ast_calc_type(ast->bin_node2, env);
		if(ast->bin_node2->type){
		    ast->type = type_slice(ast->bin_node2->type,
			    ast->bin_node1->id);
		}
		break;


	    case AST_RETURN:
		ast_calc_type(ast->una_node,env);
		struct type* ret;
		ret = env_lookup_type(env, "ret");
		if(ret && type_eq(ret, ast->una_node->type)){
		    ast->type = type_unit();
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_LET2://The second is a type
		ast_calc_type(ast->bin_node2, env);
		if(judge_is_var(ast->bin_node2)){
		    ast_calc_var(ast->bin_node2, env);
		}
		if(ast->bin_node1->kind == AST_PAT_MUT_ID){
		     type_insert(ast->bin_node1->una_node->id,
			type_mut(ast->bin_node2->type), env);
		}
		else{
		    type_insert(ast->bin_node1->una_node->id,
			    type_mut(ast->bin_node2->type), env);
		}
		ast->type = type_unit();
		break;

	    case AST_LET3:
		ast_calc_type(ast->ter_node2, env);
	        if(judge_is_var(ast->ter_node2)){
		    ast_calc_var(ast->ter_node2, env);
		}
		ast_calc_type(ast->ter_node3, env);
		if(type_eq(ast->ter_node2->type, ast->ter_node3->type)){
		    ast->type = type_unit();
		    // whether the type is MUT?
		    if(ast->ter_node1->kind==AST_PAT_MUT_ID){
			type_insert(ast->ter_node1->una_node->id,
			    type_mut(ast->ter_node2->type),env);
		    }
		    else{
			type_insert(ast->ter_node1->una_node->id,
			     ast->ter_node2->type, env);
		    }

		}
		else{
		    ast->type = type_invalid();
		    type_insert(ast->ter_node1->una_node->id,
			type_invalid(), env);
		}
		break;

	    case AST_LET4://The second is an exp
		ast_calc_type(ast->bin_node2, env);
		if(type_is_invalid(ast->bin_node2->type)){
		    ast->type = type_invalid();
		    type_insert(ast->bin_node1->una_node->id,
			type_invalid(), env);
		}
		else{
		    ast->type = type_unit();
		    if(ast->bin_node1->kind==AST_PAT_MUT_ID){
			type_insert(ast->bin_node1->una_node->id,
				type_mut(ast->bin_node2->type), env);
		    }
		    else{
			type_insert(ast->bin_node1->una_node->id,
				ast->bin_node2->type, env);
		    }
		}
		break;

	    case AST_PAT_MUT_ID:
		ast_calc_type(ast->una_node, env);
		if(ast->una_node->type){
		    ast->type = type_mut(ast->una_node->type);
		    ast->id = ast->una_node->id;// mut also has id
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_STMT_EXP:
		ast_calc_type(ast->una_node, env);
		if(!type_is_invalid(ast->una_node->type))
		    ast->type = type_unit();
		else
		    ast->type = type_invalid();
		break;	

	    case AST_ADD:
	    case AST_SUB:
	    case AST_MUL:
	    case AST_DIV:
	    case AST_REM:
		ast_calc_type(ast->bin_node1, env);
		ast_calc_type(ast->bin_node2, env);
		if(type_is_i32(ast->bin_node1->type)&&
			type_is_i32(ast->bin_node2->type)){
		    ast->type = type_i32();
		}
		else{
		    ast->type = type_invalid();
		}
		break;
	   
	    case AST_OR:
	    case AST_AND:
		ast_calc_type(ast->bin_node1,env);
		ast_calc_type(ast->bin_node2,env);
		if(type_is_bool(ast->bin_node1->type)&&
			type_is_bool(ast->bin_node2->type)){
		    ast->type = type_bool();
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_NOT:
		ast_calc_type(ast->una_node,env);
		if(type_is_bool(ast->una_node->type)){
		    ast->type = type_bool();
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_LT:
	    case AST_GT:
	    case AST_LEQ:
	    case AST_GEQ:
		ast_calc_type(ast->bin_node1,env);
		ast_calc_type(ast->bin_node2,env);
		if(type_is_i32(ast->bin_node1->type)
			&&type_is_i32(ast->bin_node2->type)){
		    ast->type = type_bool();
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_EQ:
	    case AST_NEQ:
		ast_calc_type(ast->bin_node1, env);
		ast_calc_type(ast->bin_node2, env);
		if(type_eq(ast->bin_node1->type,
			    ast->bin_node2->type)
			&& !type_eq(ast->bin_node1->type, type_invalid())){
		    ast->type = type_bool();
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_ADDR_OF:
		ast_calc_type(ast->una_node, env);
		assert(ast->una_node->type);
		ast->type = type_ref(ast->una_node->type);
		break;

	    case AST_DEREF:
		ast_calc_type(ast->una_node, env);
		assert(ast->una_node->type);
		ast->type = ast->una_node->type->type;
		break;

	    case AST_FIELD_LOOKUP:
		ast_calc_type(ast->bin_node1, env);
		if(ast->bin_node1->type->kind == TYPE_MUT){
		    assert(ast->bin_node1->type->type);
		    ast->type = type_mut(
			    struct_find_type(ast->bin_node1->type->type,
			    ast->bin_node2->id));
		}
		else{
		    assert(ast->bin_node1->type);
		    ast->type = struct_find_type(ast->bin_node1->type,
			    ast->bin_node2->id);
		}
		if(ast->type==NULL){
		    ast->type = type_invalid();
		}
		break;

	    case AST_FN_CALL1:
		ast_calc_type(ast->bin_node1,env);
		ast_calc_type(ast->bin_node2,env);
		if(type_eq(ast->bin_node1->type,
			    ast->bin_node2->type)
			&& ast->bin_node1->type->params!=NULL){
		    ast->type = ast->bin_node1->type->type;
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_FN_CALL2:
		ast_calc_type(ast->una_node, env);
		if(ast->una_node->type 
			&& !ast->una_node->type->params){
		    ast->type = ast->una_node->type->type;
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_BOX_NEW:
		ast_calc_type(ast->una_node, env);
	  	list = g_list_first(ast->una_node->type->params);
		ast->type =// List of slice(in Params) 
		    type_box(((struct type*)(list->data))->type);
		break;

	    case AST_ARR_INDEX:
		if(ast->bin_node1->kind == AST_ID){    
		    ast->bin_node1->type = 
		        env_lookup_type(env, ast->bin_node1->id);
		    //ast->bin_node1->type->kind = TYPE_ARRAY_ID;
		}
		else{
		    ast_calc_type(ast->bin_node1, env);
		}
		if(ast->bin_node1->type){
		    if(type_is_mut(ast->bin_node1->type)){
			ast->type = 
			    type_mut(ast->bin_node1->type->type->type);
		    }
		    else{
		    	if(ast->bin_node1->type->type){
			    ast->type = ast->bin_node1->type->type;
		    	}
		    	else{
			    ast->type = type_invalid();
		    	}
		    }
		}
		else{
		    ast->type = type_invalid();
		}
		ast_calc_type(ast->bin_node2, env);
		break;

	    case AST_ASSIGN:
	    case AST_ASSIGN_ADD:
	    case AST_ASSIGN_SUB:
	    case AST_ASSIGN_MUL:
	    case AST_ASSIGN_DIV:
	    case AST_ASSIGN_REM:
		ast_calc_type(ast->bin_node1, env);
		ast_calc_type(ast->bin_node2, env);
		if(type_is_mut(ast->bin_node1->type)
			&& type_eq(ast->bin_node1->type,
			    ast->bin_node2->type)){
		    ast->type = type_unit();
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_LIT_DEC:
		ast->type = type_i32();
		break;

	    case AST_TYPE_REF:
		ast_calc_type(ast->una_node, env);
		assert(ast->una_node->type);
		ast->type = type_ref(ast->una_node->type);
		break;

	    case AST_TYPE_BOX:
		ast_calc_type(ast->una_node, env);
		assert(ast->una_node->type);
		ast->type = type_box(ast->una_node->type);
		break;

	    case AST_LIT_CHAR:
		ast->type = type_u8();
		break;

	    case AST_LIT_STR:
		ast->type = type_ref(type_u8());
		break;

	    case AST_TYPE_ARR1:
		length = 0;
		struct type* last_type=NULL;
		ast->type = NULL;
		for(GList *i=g_list_first(ast->una_node->list);
			i;i=i->next){
		    ast_calc_type((struct ast*)i->data, env);
		    if(last_type!=NULL){
			if(!type_eq(last_type,((struct ast*)i->data)->type)){
			    ast->type = type_invalid();
			}
		    }
		    length++;
		    last_type = ((struct ast*)i->data)->type;
		}
		if(ast->type ==NULL){
	            ast->type = type_array(last_type,length);
		}
		break;

	    case AST_STRUCT:
		ast_calc_type(ast->bin_node2, env);
		if(ast->bin_node2->type){
		    struct type* n = 
			env_lookup_var(env, ast->bin_node1->id);
		    if(type_eq(n, ast->bin_node2->type)){
			ast->type = ast->bin_node2->type;
			ast->type->id = ast->bin_node1->id;
		    }
		    else{
			ast->type = type_invalid();
		    }
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_TRUE:
	    case AST_FALSE:
		ast->type = type_bool();
		break;
	   
	    case AST_TYPE_ARR2:
		ast_calc_type(ast->bin_node1, env);
		ast->type = type_array(ast->bin_node1->type, 
			ast->bin_node2->lit_dec);
		break;

	    case AST_TYPE_UNIT:
	    case AST_UNIT:
		ast->type = type_unit();
		break;

	    case AST_TYPE_I32:
		ast->type = type_i32();
		break;

	    case AST_TYPE_U8:
		ast->type = type_u8();
		break;

	    case AST_TYPE_BOOL:
		ast->type = type_bool();
		break;
	
	    case AST_EXPRS:
		list = NULL;
		for(GList *i=g_list_first(ast->list);
			i;i=i->next){
		    ast_calc_type((struct ast*)(i->data), env);
		    if(((struct ast*)(i->data))->type){
			list = g_list_append(list, 
				type_slice(((struct ast*)(i->data))->type,
				    NULL));
		    }
		}
		ast->type = type_fn(list, TYPE_FN, NULL);
		break;

	    case AST_ID:
		ast->type = env_lookup_type(env, ast->id);
		if(judge_is_var(ast)){
		    ast->type = env_lookup_var(env, ast->id);
		}
		break;

	    case AST_BLOCK:
		ast->type = type_unit();
		new_env = env_copy(env);
	  	for(GList* i=g_list_first(ast->list);i;i=i->next){
		    ast_calc_type((struct ast*)i->data, new_env);
		    if(!type_is_unit(((struct ast*)i->data)->type)){
			type_destroy(ast->type);
			ast->type = ((struct ast*)i->data)->type;
		    }
		}
		env_free(new_env);
		break;

	    case AST_LOOP:
		ast_calc_type(ast->una_node, env);
		if(type_is_unit(ast->una_node->type)){
		    ast->type = type_unit();
		}
		else{
		    ast->type = type_invalid();
		}
		break;
	  
  	    case AST_IF1:
		ast_calc_type(ast->bin_node1, env);
		ast_calc_type(ast->bin_node2, env);
		if(type_eq(ast->bin_node1->type, type_bool())&&
			type_eq(ast->bin_node2->type, type_unit())){
		    ast->type = type_unit();
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_IF2:
		ast_calc_type(ast->ter_node1, env);
		ast_calc_type(ast->ter_node2, env);
		ast_calc_type(ast->ter_node3, env);
		if(type_eq(ast->ter_node1->type,type_bool())
			&& type_eq(ast->ter_node2->type,
			    ast->ter_node3->type)){
		    ast->type = ast->ter_node2->type;
		}
		else{
		    ast->type = type_invalid();
		}
		break;

	    case AST_WHILE:
		ast_calc_type(ast->bin_node1,env);
		ast_calc_type(ast->bin_node2,env);
		if(type_is_bool(ast->bin_node1->type)&&
			type_is_unit(ast->bin_node2->type)){
		    ast->type = type_unit();
		}
		else{
		    ast->type = type_invalid();
		}
		break;
		break;
	}
    }
}
void ast_calc_var(struct ast* ast, struct env* env){
    if(ast==NULL) return;
    switch(ast->kind){
	case AST_ID:
	    ast->type = env_lookup_var(env, ast->id);
	    break;
    }
}
bool judge_is_var(struct ast* ast){
    if(ast->kind == AST_ID){
	if(ast->type==NULL){
	    return true;
	}
	else{
	    if(type_eq(ast->type, type_invalid())){
		return true;
	    }
	}
    }
}
bool type_is_invalid(struct type* type){
    return type_eq(type, type_invalid());
}
bool type_is_error(struct type* type){
    return type_eq(type,type_error());
}
bool type_is_ok(struct type* type){
    return type_eq(type, type_ok());
}
bool type_is_unit(struct type* type){
    return type_eq(type, type_unit());
}
bool type_is_i32(struct type* type){
    return type_eq(type, type_i32());
}
bool type_is_bool(struct type* type){
    return type_eq(type, type_bool());
}
bool type_is_mut(struct type* type){
    return type && type->kind ==TYPE_MUT;
}

struct type* type_new(int kind){
    struct type * type = calloc(1, sizeof(*type));
    assert(type);
    type->kind = kind;
    return type;
}
struct type* type_mut(struct type* type){
    struct type* n = type_new(TYPE_MUT);
    n->type = type;
    return n;
}
struct type* type_invalid(){
    static struct type n = {.kind = TYPE_INVALID};
    return &n;
}
struct type* type_error(){
    static struct type n = {.kind = TYPE_ERROR};
    return &n;
}
struct type* type_ok(){
    static struct type n = {.kind = TYPE_OK};
    return &n;
}
struct type* type_unit(){
    static struct type n = {.kind = TYPE_UNIT};
    return &n;
}
struct type* type_i32(){
    static struct type n = {.kind = TYPE_I32};
    return &n;
}
struct type* type_u8(){
    static struct type n = {.kind = TYPE_U8};
    return &n;
}
struct type* type_bool(){
    static struct type n = {.kind = TYPE_BOOL};
    return &n;
}
struct type* type_div(){
    static struct type n = {.kind = TYPE_BOOL};
    return &n;
}
struct type* type_ref(struct type* type){
    struct type* n = type_new(TYPE_REF);
    n->type = type;
    return n;
}
struct type* type_array(struct type* type, int length){
    struct type* n = type_new(TYPE_ARRAY);
    n->type = type;
    n->length = length;
    return n;
}
struct type* type_slice(struct type* type, char * id){
    struct type* n = type_new(TYPE_SLICE);
    n->type = type;
    n->id = id;
    return n;
}
struct type* type_item(GList* list, int kind){
    struct type* n = type_new(kind);
    n->params = list;
    return n;
}
struct type* type_fn(GList* list, int kind, struct type* type){
    struct type* n = type_item(list, kind);
    n->type = type;
    return n;
}
struct type* type_box(struct type* type){
    struct type* n = type_new(TYPE_BOX);
    n->type = type;
    return n;
}

void type_destroy(struct type* type){
    if(!type) return;

    switch(type->kind){
	case TYPE_INVALID:
	case TYPE_ERROR:
	case TYPE_OK:
	case TYPE_UNIT:
	case TYPE_I32:
	case TYPE_U8:
	case TYPE_BOOL:
	case TYPE_DIV:
	    return;

	case TYPE_ID:
	    break;

	case TYPE_REF:
	case TYPE_MUT:
	case TYPE_SLICE:
	case TYPE_ARRAY:
	case TYPE_BOX:
	    type_destroy(type->type);
	    break;

	case TYPE_FN:
	    type_destroy(type->type);
	    //g_list_free_full(type->params,(GDestroyNotify)pair_destroy);
	    break;

	default:assert(false);
    }
    free(type);
}

bool type_eq(const struct type* left, const struct type* right){
    assert(left);assert(right);

    left = strip_mut(left);
    right = strip_mut(right);

    //assert(left);assert(right);

    if(left==right) return true;

    if(left->kind != right->kind) return false;

    switch(left->kind){
	case TYPE_INVALID:
	case TYPE_ERROR:
	case TYPE_OK:
	case TYPE_UNIT:
	case TYPE_I32:
	case TYPE_U8:
	case TYPE_BOOL:
	case TYPE_DIV:
	    return false;

	case TYPE_ID:
	    return strcmp(left->id, right->id)==0;

	case TYPE_REF:
	case TYPE_MUT:
	case TYPE_SLICE:
	case TYPE_BOX:
	    return type_eq(left->type, right->type);

	case TYPE_ARRAY:
	    return left->length == right->length &&
		type_eq(left->type, right->type);
 
	case TYPE_STRUCT:
	case TYPE_FN:
	    if(right->kind != left->kind){
		return false;
	    }
	    if(left->params && right->params){
	        GList* i=g_list_first(left->params);
		GList* j=g_list_first(right->params);
		while(i&&j){
		    if(!type_eq((struct ast*)(i->data),
			(struct ast*)(j->data))){
			return false;
		    }
		    i=i->next; 
		    j=j->next;
		    if((i&&!j)||(!i&&j)){
			return false;
		    }
		}
	    }
	    return true; 

	default: assert(false);
    }
}

struct env* env_new(){
    struct env* env = malloc(sizeof(*env));
    env->types = g_hash_table_new(g_str_hash, g_str_equal);
    env->vars = g_hash_table_new(g_str_hash, g_str_equal);
    return env;
}

void type_insert(char* key, struct type* value, struct env* env){
    insert_into(key, value, env->types);
}

void var_insert(char* key, struct type* value, struct env* env){
    insert_into(key, value, env->vars);
}

static void insert_into(char* key, struct type* value, GHashTable* ht){
    g_hash_table_insert(ht, key, value);
}

struct type* env_lookup_type(struct env* env, char* key){
    assert(env);
    struct type* type = 
	(struct type* )g_hash_table_lookup(env->types, key);
    if(!type) return type_invalid();
    else return type;
}

struct type* env_lookup_var(struct env* env, char * str){
    assert(env);

    struct type* type = NULL;
    type = (struct type*) g_hash_table_lookup(env->vars, str);
    if(! type){
	return type_invalid();
    }
    else{
	return type;
    }
}

struct env* env_copy(struct env* old){
    struct env* new = env_new();
    g_hash_table_foreach(old->types, (GHFunc)insert_copy_into, new->types);
    g_hash_table_foreach(old->vars, (GHFunc)insert_copy_into, new->vars);
    return new;
}


void env_free(struct env* env){
    assert(env);
    free(env);
}

static void insert_copy_into(char* key, struct type* value, GHashTable* ht){
    assert(value);
    g_hash_table_insert(ht, GINT_TO_POINTER(key), value);
}

static struct type* strip_mut(const struct type* type){
    if(type_is_mut(type)){
	assert(type);
	return type->type;
    }
    else{
	return (struct type*) type;
    }
}

struct type* struct_find_type(struct type* type, char* id){
    for(GList *i=g_list_first(type->params);i;
	    i=i->next){
	if(strcmp(((struct type*)(i->data))->id, id)==0){
	    return (((struct type*)(i->data))->type);
	}
    }
    return NULL;
}
