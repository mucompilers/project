#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "env.h"
#include "ast.h"
#include "parser.h"
#include "type_annotate.h"
#include <assert.h>

void type_annotate(GNode * crate, struct env * global){
	int * n = malloc(sizeof(int));	*n = 0;//used for tracking function returns
	g_hash_table_insert(global->vars , GINT_TO_POINTER(999) , (gpointer)n  );//utility

	GNode * item = crate->children->children;
	
	struct ast* data;
	GHashTable * local;
	
	struct type * type_result = get_type(crate);

	//struct type * child_result;
	int error_flag = 0;
	while(item){
		data = (struct ast *)(item->data);
		switch(data->kind){		
			case FNDEF_ITEM: 
				local = data->fields; 
				type_result = type_annotate_fndef(item, global, local);
				break;
			case STRUCTDEF_ITEM:
			case ENUMDEF_ITEM:
				type_result->kind = get_type(item)->kind;
				break;
			default:
				type_result->kind = TYPE_ERROR;
		}
		if(type_result->kind == TYPE_ERROR || type_result->kind == TYPE_INVALID){
			error_flag |= 1;
		}
		item = item->next;
	}

	if(error_flag){//if it was proven innocent mark it ok.
		get_type(crate)->kind = TYPE_ERROR;
	}
	else{
		get_type(crate)->kind = TYPE_OK;
	}

}

struct type * type_annotate_fndef(GNode * fn_def, struct env * global, GHashTable * local){
	GNode * child = fn_def->children->next;//skip the ID child node

	struct type * type_expected = new_type(TYPE_UNIT, 0, NULL); //default expectation
	struct type * type_result = get_type(fn_def); //default invalid
	while(child){
		switch(get_ast(child)->kind){
			case FNDEFRET_ITEM:
				type_expected = get_ast(child)->type;
				type_result = get_ast(child)->type;
				break;
			case BLOCK:
				type_result = type_annotate_block(child, global, local, type_expected);
				break;
			case FNDEFPARAMS_ITEM:
				break;//already done in survey_fn_def
			default:
				printf("error in typing engine\n");
				break;
		}
		child = child->next;
	}

	if(type_eq(type_expected, type_result)){
		//type_result->kind = TYPE_OK;
		return mark_type(fn_def, type_result);
	}
	else{
		return mark_type(fn_def, new_type(TYPE_ERROR, 0, NULL));
	}

}

struct type * type_annotate_block(GNode * block, struct env * global, GHashTable * local, struct type * ret_type) {
	int * n = (int *)(g_hash_table_lookup(global->vars, GINT_TO_POINTER(999)));
	//empty block case
	struct type * type_result = get_type(block);

	if(block->children == NULL) {
		return mark_type(block, new_type(TYPE_UNIT, 0, NULL));	
	}
	
	//non-empty block case
	GNode * child = block->children;
	int error_flag = 0;
	struct type * exp;//for possible expression in return values
	while(child){
		switch(get_ast(child)->kind){
			case RETURN_STMT:
				type_result = mark_type(child, new_type(TYPE_UNIT, 0, NULL));//innocent until proven guilty
				if(!type_eq(ret_type, type_result)){
					type_result = new_type(TYPE_ERROR, 0, NULL);
					mark_type(child, new_type(TYPE_ERROR, 0, NULL));
				}
				else{
					(*n)++;
					type_result = new_type(TYPE_UNIT, 0, NULL);
					mark_type(child, new_type(TYPE_UNIT, 0, NULL));
				}
				break;
			case RETURNEXP_STMT:
				exp = type_annotate_exp_stmt(child->children, global, local, NULL);
				if(!type_eq(exp, ret_type)){
					type_result = new_type(TYPE_ERROR, 0, NULL);
					mark_type(child, new_type(TYPE_ERROR, 0, NULL));
				}
				else{
					(*n)++; 
					switch(get_ast(child->parent->parent)->kind){
						case FNDEF_ITEM:					
							type_result = type_copy(ret_type);
							mark_type(child, new_type(TYPE_UNIT,0, NULL));
							break;
						default:
							type_result = exp;//new_type(TYPE_UNIT, 0, NULL);
							mark_type(child, exp);//as per assignment guidlines
					}
				}
				break;
			case LET_STMT:
				type_result = type_annotate_let_stmt(child, global, local);
				break;
			case LETTYPEASS_STMT:
				type_result = type_annotate_let_type_assign(child, global, local);
				break;
			case LETASS_STMT:
				type_result = type_annotate_let_assign(child, global, local);
				break;
			case LETTYPE_STMT:
				type_result = type_annotate_let_type(child, global, local);
				break;
			case STMT:

				type_result = type_annotate_exp_stmt(child->children, global, local, ret_type);
				if((type_result->kind == TYPE_ERROR) || (type_result->kind == TYPE_INVALID)){
					mark_type(child, new_type(TYPE_ERROR, 0, NULL));
				}
				else{
					type_result = new_type(TYPE_UNIT, 0, NULL);
					mark_type(child, type_result);
				}

				break;

			default://handle expressions
				type_result = type_annotate_exp_stmt(child, global, local, ret_type);				
		}//end switch

		if(type_result->kind == TYPE_INVALID || type_result->kind == TYPE_ERROR){// check for accum errors in children
			error_flag = 1;// so TYPE_ERROR will be propogated up
		}
		child = child->next;
	}//end while


	if(get_ast(block->parent)->kind == FNDEF_ITEM){//allows a single valid return to OK function, if ending type not same as ret
		if(   !type_eq(type_result , ret_type)  &&  (((*(n)) > 0) &&  (error_flag!=1)) ){
			*n = 0;//set return watch counter back to zero for use by future function defs
			return mark_type(block, ret_type);
		}
	}
	if((error_flag == 1)){
		return mark_type(block, new_type(TYPE_ERROR,0, NULL));
	}
	else{
		return mark_type(block, type_result);
	}

}


struct type * type_annotate_exp_stmt(GNode * exp_stmt, struct env * global, GHashTable * local, struct type * ret_type){
	//if(ret_type) printf("ret_type inside exp_stmt annotate %d\n", ret_type->kind);
	struct type * type_result = get_type(exp_stmt);
	struct ast * data = (struct ast *)(exp_stmt->data);
		switch(data->kind){
			case ID:
				//need stuff
				type_result = type_annotate_id(exp_stmt, global, local);
				break;
			//primitive literals
			case TRU:
			case FALS:
				type_result = mark_type(exp_stmt, data->type);
				break;
			case UNIT_EXP:
				type_result = mark_type(exp_stmt, data->type);
				break;
			case LITCHAR:
				type_result = mark_type(exp_stmt, data->type);
				break;
			case LITDEC:
				type_result = mark_type(exp_stmt, data->type); // mark_type(exp_stmt, data->type);

				break;
			case LITSTR:
				type_result = mark_type(exp_stmt, data->type);
				break;
			//complex literals
			case ENUM_LIT:
				type_result = type_annotate_enum_lit(exp_stmt, global, local);
				break;
			case STRUCT_LIT:
				type_result = type_annotate_struct_lit(exp_stmt, global, local);
				break;

			case ARRAY_LIT:
				type_result = type_annotate_arr_lit(exp_stmt, global, local);
				break;
		
			//binary ops
			case OR_EXP:
			case AND_EXP:
				type_result = type_annotate_binary_op_bool(exp_stmt, global, local);
				break;
			case EQ_EXP:
			case NE_EXP:
				type_result = type_annotate_equality(exp_stmt, global, local);
				break;
			case LT_EXP:
			case GT_EXP:
			case LE_EXP:
			case GE_EXP:
				type_result = type_annotate_comparison(exp_stmt, global, local);
				break;
			case ADD_EXP:
			case SUB_EXP:
			case MUL_EXP:
			case DIV_EXP:
			case REM_EXP:
				type_result = type_annotate_binary_op_i32(exp_stmt, global, local);
				break;
			//unary ops
			case NEG_EXP:
			case NOT_EXP:
				type_result = type_annotate_unary_op(exp_stmt, global, local);
				break;
			//pointers/addresses
			case ADDR_EXP:
				type_result = type_annotate_ref(exp_stmt, global, local);
				break;
			case BOXNEW_EXP:
				type_result = type_annotate_box_new(exp_stmt, global, local);
				break;
			case DEREF_EXP:
				type_result = type_annotate_deref(exp_stmt, global, local);
				break;

			//assignment operations TODO
			case PLUS_ASSIGN:
			case SUB_ASSIGN:
			case MUL_ASSIGN:
			case REM_ASSIGN:
			case DIV_ASSIGN:
				type_result = type_annotate_comp_assignment(exp_stmt, global, local);
				break;
			case ASSIGNMENT:
				type_result = type_annotate_assignment(exp_stmt, global, local);
				break;
			case IF_EXP:
				type_result = type_annotate_if(exp_stmt, global, local, ret_type);
				break;
			case WHILE_EXP:
				type_result = type_annotate_while(exp_stmt, global, local, ret_type);
				break;
			case LOOP_EXP:
				type_result = type_annotate_loop(exp_stmt, global, local, ret_type);
				break;
			case FNCALL_EXP:
				type_result = type_annotate_fncall(exp_stmt, global, local);
				break; 
			case ARRIDX_EXP:
				//get_type(exp_stmt)
				type_result = type_annotate_arr_index(exp_stmt, global, local);
				break;
			case FIELDLUP_EXP:
				type_result = type_annotate_field_lookup(exp_stmt,global, local);
				break;
		}

	return type_result;

}

struct type * type_annotate_comp_assignment(GNode * opnode, struct env * global, GHashTable * local){

	struct type * type_result = get_type(opnode);

	struct type * lval_type = type_annotate_exp_stmt(opnode->children, global, local, NULL);
	struct type * rval_type = type_annotate_exp_stmt(opnode->children->next, global, local, NULL);
	
	//if(lval_type

	if(       lval_type->kind == TYPE_MUT && rval_type->kind != TYPE_ERROR && 
		type_eq(lval_type, rval_type) && stc(lval_type->type, TYPE_I32))
	{
		type_result->kind = TYPE_UNIT;
	}
	else{
		type_result->kind = TYPE_ERROR;
	}
	
	return mark_type(opnode, type_result);	
}

struct type * type_annotate_assignment(GNode * opnode, struct env * global, GHashTable * local){
	struct type * type_result = get_type(opnode);

	struct type * lval_type = type_annotate_exp_stmt(opnode->children, global, local, NULL);
	struct type * rval_type = type_annotate_exp_stmt(opnode->children->next, global, local, NULL);

	if( lval_type->kind == TYPE_MUT  &&  rval_type->kind!=TYPE_ERROR && type_eq(lval_type, rval_type) )
	{
		type_result->kind = TYPE_UNIT;
	}
	else{
		type_result->kind = TYPE_ERROR;
	}

	return mark_type(opnode, type_result);	
}


struct type * type_annotate_ref(GNode * ref, struct env * global, GHashTable * local){
	struct type * type_result = new_type(TYPE_INVALID, 0, NULL);
	struct type * exp_type;
	
	exp_type = type_annotate_exp_stmt(ref->children, global, local, NULL);
	
	switch(get_ast(ref->children)->kind){
		case ID://allow dereferencing of these
		case ARRIDX_EXP:
		case FIELDLUP_EXP:
		case ADDR_EXP:
		case BOXNEW_EXP:
		case DEREF_EXP:
			type_result->kind = TYPE_REF;
			type_result->type = type_copy(exp_type);
			break;
		default://don't allow derefercing of any other
			type_result = new_type(TYPE_ERROR, 0, NULL);
	}	
	
	return mark_type(ref, type_result);
}

struct type * type_annotate_deref(GNode * deref, struct env * global, GHashTable * local){
	struct type * type_result = get_type(deref);
	struct type * exp_type;

	exp_type = type_annotate_exp_stmt(deref->children, global, local, NULL);	
	switch(get_type(deref->children)->kind){
		case TYPE_REF:
			if(exp_type->kind != TYPE_ERROR){
				type_result = get_type(deref->children); //type as in the t of &t
			}
			else{
				type_result->kind = TYPE_ERROR;
			}
			break;
		case TYPE_BOX:
			if(exp_type->kind != TYPE_ERROR){
				type_result = exp_type->type; //type as in the t of &t
			}
			else{
				type_result->kind = TYPE_ERROR;
			}
			break;
		default:
			type_result->kind = TYPE_ERROR;
	}	

	return mark_type(deref, type_result);
}

struct type * type_annotate_box_new(GNode * box, struct env * global, GHashTable * local){
	
	struct type * type_result;
	
	type_result = type_annotate_exp_stmt(box->children->children, global, local, NULL);

	mark_type(box->children, type_result);	

	if(type_result->kind != TYPE_ERROR){
		get_type(box)->type = type_copy(type_result);
		get_type(box)->kind = TYPE_BOX;
	}

	return get_type(box);
}





struct type* type_annotate_arr_index(GNode * arr, struct env * global, GHashTable * local){

		
	struct type *t,*t_i32;
	t = type_annotate_exp_stmt(arr->children, global, local, NULL);
	t_i32 = type_annotate_exp_stmt(arr->children->next, global, local, NULL);
	
	struct type* not_stripped = t;
	//if(t->kind == TYPE_MUT){
	t = strip_mut(t);
	t_i32 = strip_mut(t_i32);

	if( !t ){
		get_type(arr)->kind = TYPE_ERROR; //printf("clause 1\n");
	}
	//Can't index a type that is not an array
	else if(t->kind != TYPE_ARRAY ||  t_i32->kind != TYPE_I32 ){
		get_type(arr)->kind = TYPE_ERROR; //printf("clause 2\n");
	}
	//Let's mark the type on this sucker!
	else {
		if( not_stripped->kind == TYPE_MUT){
			//struct type * mut = new_type(TYPE_MUT, 0, NULL);
			mark_type(arr, t);
			get_type(arr)->kind = TYPE_MUT;
		}
		else{
			mark_type(arr, t->type);
		}
	}

	return 	get_type(arr);

}


struct type*  type_annotate_let_stmt(GNode* let,struct env* global,GHashTable *local){
	
	if(!local)
		printf("ERRRRRROR\n");
	
		switch(get_ast(let)->kind){

                        case LETTYPEASS_STMT:
				return type_annotate_let_type_assign( let ,  global , local);
				break;
                        case LETASS_STMT:
				return type_annotate_let_assign( let ,  global , local);
				break;
                        case LETTYPE_STMT:
				return type_annotate_let_type( let ,  global , local);
				break;
			case LET_STMT:
				//Do nothing
				break;
	 		default:
				return NULL;
		}
	

	return NULL;
}


struct type*  type_annotate_let_type_assign(GNode* let,struct env* global,GHashTable *local){

	GNode* type = let->children->next;
	GNode* pat = let->children;
	GNode* exp = let->children->next->next;	
	GNode* id = pat->children;


	struct type* t_type = type_annotate_complex_type(type, global);
	struct type* t_exp  = type_annotate_exp_stmt(exp, global, local, NULL);
	
	unsigned int qid = g_quark_from_string(  get_ast( id  )->str  );

	//If the types aren't the same
	if( !type_eq( t_type , t_exp) )	
		get_type(let)->kind = TYPE_ERROR;
	
	//If the variable doesn't exist yet
	else if(  1/*!g_hash_table_lookup( local , GINT_TO_POINTER(qid)  )*/   ){
			
		get_type(let)->kind = TYPE_UNIT;	
		
		
		if(get_ast(pat)->kind == MUT_PAT){
			get_type(id)->kind = TYPE_MUT;
			get_type(id)->type = type_copy(t_type);			
			get_type(id)->params = get_type(id)->type->params;//Copy up params from below	
			get_ast(id->parent)->type = type_copy(get_type(id)->type);
		}
		else
			mark_type( id , t_type );

		if(t_exp->kind == TYPE_ERROR){
			get_type(let)->kind = TYPE_ERROR;
		}
	
		g_hash_table_insert( local , GINT_TO_POINTER(qid) , (gpointer)get_type(id) );
	
	}

	//Variable exists
	else
		get_type(let)->kind = TYPE_ERROR;

	
	return get_type(let);
}


struct type*  type_annotate_let_assign(GNode* let,struct env* global,GHashTable *local){
		
	
	GNode* exp = let->children->next;
	GNode* pat = let->children;
	GNode* id = pat->children;

	struct type* t_exp  =  type_annotate_exp_stmt(exp , global , local, NULL);
	
	unsigned int qid = g_quark_from_string(  get_ast( id  )->str  );

	//If the variable doesn't exist yet
	if(  1/*!g_hash_table_lookup( local, GINT_TO_POINTER(qid)  ) */  ){
			
		get_type(let)->kind = TYPE_UNIT;	
		
		
		if(get_ast(pat)->kind == MUT_PAT){
			get_type(id)->kind = TYPE_MUT;
			get_type(id)->type = type_copy(t_exp);
			get_type(id)->params = get_type(id)->type->params;//Copy up params from below
			get_ast(id->parent)->type = type_copy(get_type(id)->type);
		}
		else
			mark_type( id , t_exp );

		if(t_exp->kind == TYPE_ERROR){
			get_type(let)->kind = TYPE_ERROR;
		}
	
		g_hash_table_insert( local , GINT_TO_POINTER(qid) , (gpointer)get_type(id) );
	
	}
	//Variable exists
	else
		get_type(let)->kind = TYPE_ERROR;

	
	return get_type(let);
	

}


struct type*  type_annotate_let_type(GNode* let,struct env* global,GHashTable *local){

	GNode* type = let->children->next;
	GNode* pat = let->children;
	GNode* id = pat->children;

	struct type* t_exp  =  type_annotate_complex_type(type, global);

	unsigned int qid = g_quark_from_string(  get_ast( id  )->str  );

	//If the variable doesn't exist yet
	if(  1/*!g_hash_table_lookup( local, GINT_TO_POINTER(qid) )*/   ){
			
		get_type(let)->kind = TYPE_UNIT;	
		
		
		if(get_ast(pat)->kind == MUT_PAT){
			get_type(id)->kind = TYPE_MUT;
			get_type(id)->type = type_copy(t_exp);
			get_type(id)->params = get_type(id)->type->params;//Copy up params from below
			get_ast(id->parent)->type = type_copy(get_type(id)->type);
		}
		else
			mark_type( id , t_exp );

		
	
		g_hash_table_insert( local , GINT_TO_POINTER(qid) , (gpointer)get_type(id) );
	
	}
	//Variable exists
	else
		get_type(let)->kind = TYPE_ERROR;

	
	return get_type(let);

}


struct type* type_annotate_field_lookup(GNode *flup, struct env* global, GHashTable *local){

	struct type * t_flup, *t_struct;
	GNode* left = flup->children;
	GNode* right = flup->children->next;
	GHashTable* new_local;
	struct type *tmp;

	//Retrieve the type of the left child (must be a struct)
	t_struct = type_annotate_exp_stmt( left , global , local , NULL);

	struct type *stripped = strip_mut(t_struct);	

	if(stripped->params){
		
		if( get_ast( stripped->params)->kind == FNDEF_ITEM)		
			stripped = get_type( get_fndef_return( stripped->params )->children  );
		else if(get_ast(stripped->params)->kind == STRUCTDEFFIELD_ITEM  )
			;
		else{
			get_type(flup)->kind = TYPE_ERROR;
			return get_type(flup);
		}
	}
	else{
		get_type(flup)->kind = TYPE_ERROR;
		return get_type(flup);
	}


	new_local = get_ast( stripped->params )->fields;

	
	//Retrieve the type of the right child (will be the type of the flup)
	t_flup = type_annotate_exp_stmt( right , global , new_local, NULL );
	

	if(get_type(flup)->kind != TYPE_ERROR){
		if(t_struct->kind == TYPE_MUT){	
			tmp = get_type(flup);			
			get_ast(flup)->type = new_type(TYPE_MUT, 0, NULL);
			free_type(tmp);
			get_type(flup)->type = type_copy(t_flup);
			get_type(flup)->params = t_flup->params;				
		}
		else{
			mark_type(flup, t_flup);
		}
					
	}
	else
		get_type(flup)->kind = TYPE_ERROR;
	

	return get_type(flup);
}



struct type* type_annotate_id(GNode* id, struct env* global, GHashTable* local){

	
	unsigned int qid = g_quark_from_string(  get_ast( id )->str  );
	struct type* t_any = g_hash_table_lookup( local , GINT_TO_POINTER( qid ) );

	if(!t_any)
		get_type(id)->kind = TYPE_ERROR;
	else
		mark_type(id, t_any);
	
	if( get_type(id)->params  ){
		//printf("<%s> has params!!!\n", get_ast(id)->str   );	
	}	
	return get_type(id);
	
}



struct type * type_annotate_binary_op_i32(GNode * opnode, struct env * global, GHashTable * local){
	struct type * type_result = get_type(opnode);

	GNode * left = opnode->children;
	GNode * right = opnode->children->next;

	struct type * l_exp_type;
	struct type * r_exp_type;
	
	struct type * ti32 = new_type(TYPE_I32, 0, NULL);//for type comparison
	 
	l_exp_type = type_annotate_exp_stmt(left, global, local, NULL); //could even be type_annotate_block i think
	r_exp_type = type_annotate_exp_stmt(right,global, local, NULL);

	//printf("tests : %d, %d\n", type_eq(l_exp_type,r_exp_type), type_eq(l_exp_type, ti32));
	if(type_eq(l_exp_type,r_exp_type) && type_eq(l_exp_type, ti32)){
		type_result = l_exp_type;
	}
	else{
		type_result = new_type(TYPE_ERROR, 0, NULL);
	}
	
	free_type(ti32);//freed from type comparison
	return mark_type(opnode, type_result);
}

struct type * type_annotate_binary_op_bool(GNode * opnode, struct env * global, GHashTable * local){
	struct type * type_result = get_type(opnode);

	GNode * left = opnode->children;
	GNode * right = opnode->children->next;

	struct type * l_exp_type;
	struct type * r_exp_type;
	 
	struct type * tbool = new_type(TYPE_BOOL, 0, NULL);//for type comparison

	l_exp_type = type_annotate_exp_stmt(left,global,local, NULL); //could even be type_annotate_block i think
	r_exp_type = type_annotate_exp_stmt(right,global,local, NULL);

	if(type_eq(l_exp_type,r_exp_type) && type_eq(l_exp_type,tbool)){
		type_result = l_exp_type;
	}
	else{
		type_result = new_type(TYPE_ERROR, 0, NULL);
	}

	free_type(tbool);//freed from type comparison

	return mark_type(opnode, type_result);

}

struct type * type_annotate_comparison(GNode * comp, struct env * global, GHashTable * local){
	struct type * type_result = get_type(comp);
	struct type * exp_type[2];
	
	struct type * ti32 = new_type(TYPE_I32, 0, NULL);//for type comparison

	GNode * child = comp->children;
	exp_type[0] = type_annotate_exp_stmt(child, global, local, NULL);
	child = child->next;
	exp_type[1] = type_annotate_exp_stmt(child, global, local, NULL);
	if(type_eq(exp_type[0],exp_type[1]) && type_eq(exp_type[0], ti32)){
		type_result = new_type(TYPE_BOOL, 0, NULL);	
	}
	else{
		type_result = new_type(TYPE_ERROR, 0, NULL);
	}

	free_type(ti32);//freed from type comparison
	return mark_type(comp, type_result);
}

struct type * type_annotate_equality(GNode * equality, struct env * global, GHashTable * local){
	struct type * type_result = get_type(equality);

	struct type * exp_type[2];
	
	GNode * child = equality->children;
	exp_type[0] = type_annotate_exp_stmt(child, global, local, NULL);

	child = child->next;
	exp_type[1] = type_annotate_exp_stmt(child, global, local, NULL);

	if(type_eq(exp_type[0],exp_type[1])){
		type_result = new_type(TYPE_BOOL, 0, NULL);	
	}
	else{
		type_result = new_type(TYPE_ERROR, 0, NULL);
	}


	return mark_type(equality, type_result);
}



struct type * type_annotate_unary_op(GNode * opnode, struct env * global, GHashTable * local){
	struct type * type_result = get_type(opnode);
	GNode * child = opnode->children;
	struct type * exp_type = type_annotate_exp_stmt(child, global, local, NULL);

	struct type * tbool = new_type(TYPE_BOOL, 0, NULL);//for type comparison
	
	if(type_eq(exp_type, tbool)){//arithmetic negation isn't used so we just assume if bool then good 
		type_result = exp_type;
	}
	else{
		type_result = new_type(TYPE_ERROR, 0, NULL);
	}

	free_type(tbool);//freed from type comparison
	return mark_type(opnode, type_result);
}

struct type* type_annotate_fncall(GNode* fn_call, struct env* global, GHashTable * local){

	struct type* fndef_type = lookup_var( global , fn_call );
	//type_print(fndef_type);
	GNode* fncall_param, *fndef_param;	

	if( !fndef_type ){
		//printf("FNCALL: no function def found\n");
		 struct type* tmp = get_type(fn_call);	
		 tmp->kind = TYPE_ERROR; 
		 return tmp;
	}
	
		
	//Get function definition parameters
	fndef_param = get_fndef_params( fndef_type->params );
	fncall_param = get_fncall_params(fn_call); 
		
	if(!fndef_param){
		//printf("no function definition params for function of type: \n" );
		type_print( fndef_type );
	}
	//if(!fncall_param);
		//printf("no function call params\n");
	while(1){
		//Check if we have finished		
		if( !fncall_param  &&  !fndef_param )	
			break;

		//Checking if one has more parameters than the other
		if(  ( fncall_param == NULL && fndef_param != NULL )||  
		     ( fncall_param != NULL && fndef_param == NULL  )){
			//printf("FNCALL: Incorrect number of args\n");
		 	get_type(fn_call)->kind = TYPE_ERROR; 
		 	return get_type(fn_call);
		}
		
		//Check for equality		
		if( !type_eq( type_annotate_exp_stmt(fncall_param, global, local, NULL) , get_type(fndef_param)  )  ){		
		 	//printf("FNCALL: Incorrect Argument Type");
			get_type(fn_call)->kind = TYPE_ERROR; 
		 	return get_type(fn_call);
		}
			
		fndef_param = fndef_param->next;
		fncall_param = fncall_param->next;		
	}
	
	return mark_type(fn_call ,  fndef_type);

}

/////////////////////////////////
//TYLER//////////////////////////
/////////////////////////////////
struct type * type_annotate_struct_lit(GNode * struct_lit, struct env * global, GHashTable * local){
	
	struct type * type_result = get_type(struct_lit);
	
	struct type * struct_type = lookup_type(global , struct_lit);
	
	
	if(!struct_type){
		type_result->kind = TYPE_ERROR;
		return type_result;
	}	
	else
		mark_type(struct_lit, struct_type);


	type_result = get_type(struct_lit);
	type_result->kind = TYPE_ID;	

	GNode *lit_field = struct_lit->children->next->children; 	
	
	GNode *def_field = struct_type->params;
		
	GNode *def_orig = def_field;
	GNode *lit_orig = lit_field;	

	GHashTable *table = g_hash_table_new(NULL , NULL);
	
	while(lit_field){

		//Make sure this field hasn't been declared already
		unsigned int qid = g_quark_from_string( get_ast(lit_field->children)->str  );
		if(  g_hash_table_lookup(table , GINT_TO_POINTER(qid))  ){	
			type_result->kind = TYPE_ERROR;	
			printf("table error!\n");
		}
		else
			g_hash_table_insert( table , GINT_TO_POINTER(qid) , (gpointer)lit_field );		
	
		//Mark this field's type	
		mark_type(lit_field, type_annotate_exp_stmt( lit_field->children->next , global , local, NULL ) );		

		def_field = def_orig;	//reset definition field back to beginning
		while(def_field){

			//Find this field in field definitions
			//If you find it, check for type equality			
			if( !strcmp( get_ast(lit_field->children)->str , get_ast(def_field->children)->str )  ){
			
				if(  type_eq( get_type(lit_field) , get_type(def_field)  )  )
					break;
				else{
					type_result->kind = TYPE_ERROR;
					//printf("equality error!\n");
				}	
			}
			else if(  !(def_field->next)  ){//If we didn't find a match 		
				type_result->kind = TYPE_ERROR;
				//printf("no match error!\n");
			}
			def_field = def_field->next;		
		}
		

		
		lit_field = lit_field->next;
	}

	if(!def_orig)
		printf("def_orig NULL\n");

	//Compare for same field lengths
	if( child_count(lit_orig->parent) != child_count(def_orig->parent) ){
		type_result->kind = TYPE_ERROR;		
		//printf("size error!\n");
	}
	//printf("%d : %d\n", child_count(lit_orig->parent) , child_count(def_orig->parent));
	
	g_hash_table_destroy(table);
	
	return type_result;
}



struct type * type_annotate_enum_lit(GNode * enum_lit, struct env * global, GHashTable * local) {
	
	GNode * ctor;

	struct type * type_result = get_type(enum_lit);	

	//enum name validation
	struct type * hashed_type = lookup_type(global, enum_lit);
	
	if( !hashed_type  ){//if no such enum in enviroment
		type_result->kind = TYPE_ERROR;
		return mark_type(enum_lit, type_result);
	}
	else{
		mark_type(enum_lit, hashed_type);
	}
	
	type_result = get_type(enum_lit);//because earlier mark_type makes a new type for enum_lit
	get_type(enum_lit)->kind = TYPE_ID;

	//ctor name validation
	//get the ctor hash table to check if "ctorname" of this enum lit is valid
	char * ctorname = get_ast(enum_lit->children->children->next)->str;
	unsigned int qid = g_quark_from_string( ctorname );
	ctor = g_hash_table_lookup(get_ast(hashed_type->params)->fields, GINT_TO_POINTER(qid));
	//printf("test: %d, %d\n", get_ast(ctor)->kind, ENUMDEFFIELD_ITEM); //verified

	if( !ctor ){//no such constructor named
		type_result->kind = TYPE_ERROR;
		return mark_type(enum_lit, type_result);
	}

	//ctor argument validation (if there are any ctor arguments)
	if(ctor->children->next && enum_lit->children->next){
		GNode * ctortypenode = ctor->children->next->children;
		GNode * ctorexp = enum_lit->children->next->children;//target exps
		int cnt1,cnt2; cnt1=cnt2=0;
		while(ctorexp&&ctortypenode){	
			if(! type_eq( type_annotate_exp_stmt(ctorexp, global, local, NULL), get_type(ctortypenode) ) ){
				get_type(enum_lit)->kind = TYPE_ERROR;
				return mark_type(enum_lit, type_result);
			}

			ctortypenode = ctortypenode->next;
			ctorexp = ctorexp->next;
			if(ctortypenode)cnt1++;//make sure ctor arg lists correspond in length
			if(ctorexp)cnt2++;
			if(cnt1!=cnt2){
				get_type(enum_lit)->kind = TYPE_ERROR;
				return mark_type(enum_lit, type_result);
			}
		}
	}

	return type_result;
}




struct type * type_annotate_if(GNode * ifnode, struct env * global, GHashTable * local, struct type * ret_type){

	//if(ret_type) printf("ret_type inside if annotate %d\n", ret_type->kind);

	int block_counter = 0;
	struct type * block_type[2];
	struct type * exp_type;	
	struct type * type_result = get_type(ifnode);

	GNode * child = ifnode->children;

	exp_type = type_annotate_exp_stmt(child, global, local, NULL);

	
	child = child->next;
	while(child){
		block_type[block_counter] = type_annotate_block(child, global, local, ret_type);
		block_counter++;
		child = child->next;
	}

	if(block_counter == 1){//if stmt
		if(stc(exp_type, TYPE_BOOL) && block_type[0]->kind != TYPE_ERROR){
				type_result = new_type(TYPE_UNIT, 0, NULL);		
			}
		else{
			type_result = new_type(TYPE_ERROR, 0, NULL);
		}
		
	}
	else if(block_counter == 2){//if-else stmt
		if(stc(exp_type, TYPE_BOOL) && type_eq(block_type[0], block_type[1])){
				type_result = block_type[0];
			}
		else{
			type_result = new_type(TYPE_ERROR, 0, NULL);
		}
	
	}	
	else{
		printf("invalid looping in type_annotate_if()\n");
		exit(-1);
	}
	
	return mark_type(ifnode, type_result);
}


struct type * type_annotate_while(GNode * whilenode, struct env * global, GHashTable * local, struct type * ret_type){
	struct type * type_result = get_type(whilenode);
	struct type * exp_type;
	struct type * block_type;
	
	GNode * child = whilenode->children;
	exp_type = type_annotate_exp_stmt(child, global, local, NULL);

	child = child->next;
	block_type = type_annotate_block(child, global, local, ret_type);

	if(stc(exp_type, TYPE_BOOL) && stc(block_type, TYPE_UNIT)){
		type_result = new_type(TYPE_UNIT, 0, NULL);
	}
	else{
		type_result = new_type(TYPE_ERROR, 0, NULL);
	}
	
	return mark_type(whilenode, type_result);
}

struct type * type_annotate_loop(GNode * loopnode, struct env * global, GHashTable * local, struct type * ret_type){
	struct type * type_result = get_type(loopnode);
	struct type * block_type;

	GNode * child = loopnode->children;

	block_type = type_annotate_block(child, global, local, ret_type);

	if(stc(block_type, TYPE_UNIT)){
		type_result = block_type;
	}
	else{
		type_result = new_type(TYPE_ERROR, 0, NULL);
	}

	return mark_type(loopnode, type_result);
}

//Type annotate an array literal
//Type annotate the array's elements
//Compare the elements for equality
//Return:  An error type on non matching nodes
//	   or the type of the node upon
struct type * type_annotate_arr_lit(GNode* arr, struct env* global, GHashTable * local){

	struct type * mytype;
	GNode* elmt, *elmt_orig;	
	struct ast* arr_ast = get_ast(arr);	

	
	//Get array elements
	elmt = arr->children;
		
	//Save element pointer for later
	elmt_orig = elmt;

	//Recursively type child
	while(elmt){

		type_annotate_exp_stmt(elmt , global , local, NULL);
		
		elmt = elmt->next;
	}
	
	elmt = elmt_orig;
	GNode* elmt_nxt = elmt->next;
	
	//Compare all elements for equality
	while(elmt_nxt){
			
		if( !type_eq( get_type(elmt) , get_type(elmt_nxt)  ) ){			
			arr_ast->type = new_type(TYPE_ERROR, 0 , NULL);
			return arr_ast->type;
		}

		elmt = elmt->next;
		elmt_nxt = elmt->next;
	}	


	//Annotate the type with TYPE_ARRAY, the number of elements, and a null string
	arr_ast->type = new_type( TYPE_ARRAY, child_count(arr) , NULL );					
	
	//Append the type of element to the type of the array
	mytype = get_type(arr);
	mytype->type = type_copy( get_type(elmt) );	
	
	return mytype;	
}

//Count the children of a parent node
//Returns number of children the parent node has
//Returns -1 if the parent is NULL
int child_count(GNode* parent){

	if(!parent){
		return -1;
	}

	int cnt = 0;
	GNode* child = parent->children;

	while(child){
		cnt++;
		child = child->next;
	}

	return cnt;
}

struct type * mark_type(GNode * node, struct type * marker){

	struct ast * data = (struct ast *)(node->data);
	struct type * temp = data->type;
	data->type = type_copy(marker);
	if(temp)	
		free_type(temp);
 
	return data->type;	
}


struct type * type_copy( struct type * old) {//written by Chris Hathorn
      assert(old);

      switch (old->kind) {
            case TYPE_INVALID:
            case TYPE_ERROR:
            case TYPE_OK:
            case TYPE_UNIT:
            case TYPE_I32:
            case TYPE_U8:
            case TYPE_BOOL:
            case TYPE_DIV: {
		  struct type* new = calloc(1, sizeof(*new));
                  *new = *old;
		   new->id = deepcopy(old->id);
                  return new;
            }

            case TYPE_ID: {
                  struct type* new = calloc(1, sizeof(*new));
                  *new = *old;
                  return new;
            }
            case TYPE_REF:
            case TYPE_MUT:
            case TYPE_SLICE:
            case TYPE_ARRAY:
            case TYPE_BOX: {
                  struct type* new = calloc(1, sizeof(*new));
                  *new = *old;
                  new->type = type_copy(old->type);
		  new->id = deepcopy(old->id);
                  return new;
            }

            case TYPE_FN: {
                  struct type* new = calloc(1, sizeof(*new));
                  *new = *old;
		  new->id = deepcopy(old->id);
                  assert(0); 
                  return new;
            }

            default: assert(0);
      }
	return NULL;
}

int stc(struct type * node, int type){//simple type compare

	if(node->kind == type){
		return 1;
	}
	else{
		return 0;
	}
}

struct type * strip_mut( struct type * type){
	if( type->kind == TYPE_MUT){
		return type->type;
	}
	else{
	 	return type;
	}
}	

int type_eq( struct type* left, struct type* right) {//written by Christ Hathorn

      assert(left); assert(right);

      left = strip_mut(left);
      right = strip_mut(right);

      assert(left); 
      assert(right);

      if (left == right) return 1;

      if (left->kind != right->kind) return 0;

      switch (left->kind) {
            case TYPE_INVALID:
            case TYPE_ERROR:
		  return 0;
            case TYPE_OK:
            case TYPE_UNIT:
            case TYPE_I32:
            case TYPE_U8:
            case TYPE_BOOL:
		  return 1;
            case TYPE_DIV:
                  return 0;
            case TYPE_ID:
                  return !strcmp(left->id,right->id);
            case TYPE_REF:
            case TYPE_MUT:
            case TYPE_SLICE:
            case TYPE_BOX:
                  return type_eq(left->type, right->type);

            case TYPE_ARRAY://printf("hereTYPE_ARRAY in type_eq, %d, %d\n", left->length, right->length);
                  return (left->length == right->length) && type_eq(left->type, right->type);

            case TYPE_FN: {
                  return type_eq(left->type, right->type);
            }
            default: assert(0);
      }
}

///////////////////
//CONSTRUCTORS
///////////////////


struct type* new_type(int kind, int length, char* id){

	struct type *new = calloc(1, sizeof(struct type) );
	new->kind = kind; 
	new->length = length;
	
	if(id){ 
		new->id = (char*)malloc(sizeof(char) * ( strlen(id)+1 ) );	
		strcpy(new->id, id);
	}
	
	return new;
}

struct type* get_type(GNode * node){
	struct ast * data = (struct ast *)(node->data);
	return data->type;
}

struct ast * get_ast(GNode * node){
	struct ast * data = (struct ast *)(node->data);
	return data;
}



///Below from coding session two

//////////////////
//DESTRUCTORS
//////////////////



void free_type(struct type* types){


	while(types){

		if(types->id)
			free(types->id);		
		
		struct type * temp = types;	
		types = types->type;
		
		free(temp);
	}
}

void free_ast_node(struct ast* ast){

	//if(ast->str)
		//free(str);//IS problematic!! str is in a union
	
	if(ast->fields)
		g_hash_table_destroy(ast->fields);				

	free_type(ast->type);

}

void free_ast(GNode* root){

	GNode* child = root->children;
				
	while(child){
		
		GNode * tmp = child;
		child = child->next;
		free_ast(tmp);	
	
	}
	
	free_ast_node( get_ast(root) );

}

void free_nary(GNode* root){
	if(root){
		free_ast(root);
		g_node_destroy(root);
	}
}



//Any complex type can be created using this function
struct type* type_annotate_complex_type(GNode * type_node, struct env* global){

	//Top Node is Null
	if(!type_node)	{					
		return NULL;
	}
	
	
	
	struct type* t = get_type(type_node);
	struct type* lup;
	

	if(get_ast(type_node)->type->kind == TYPE_ARRAY){
		t->length = get_ast(type_node->children->next)->num;
	}
		
	
	if( get_type(type_node)->kind == TYPE_ID ){

		lup = lookup_type(global , type_node);//looking up type id 
		if( !lup ){
			t->kind = TYPE_ERROR;
			return t;
		}
		else{
			mark_type(type_node , lup);
			get_type(type_node)->kind = TYPE_ID;
		}
	}
		

	

	//If at the Bottom node		
	if( !(type_node->children) ){
		return get_type(type_node);
	}

	//Give each node a copy of it's compound type
	get_type(type_node)->type = type_copy(  type_annotate_complex_type(type_node->children, global)  );
	
	if(t->type->kind == TYPE_ERROR)
		get_type(type_node)->kind = TYPE_ERROR;
	
	return get_type(type_node);
}	



	
	
GNode* get_fncall_params(GNode* fncall){

	GNode* child;

	if(!fncall)
		return NULL;

	child = fncall->children;
	
	if(!child)
		return NULL;	

	while( child && get_ast(child)->kind != FNCALLPARAMS_EXP ){
	
		child = child->next;	
	
	}
	
	if(child)
		return child->children;
	else 
		return child;
	
}



char * deepcopy(char* old){

	if(!old)
		return NULL;	
		
	char* new = (char*)malloc(  sizeof(char) * (strlen(old)+1) );

	return strcpy(new, old);
}





