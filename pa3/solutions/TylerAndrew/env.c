#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "env.h"
#include "ast.h"
#include "parser.h"
#include "type_annotate.h"
//#include "type.h"
//Definition(fn, struct, enum) nodes have hash tables for looking up fields and parameters


/*
 *Looking up a variable in the environment:
 *	-Key is the identifier
 *	-Lookup a variable based on what kind of node it belongs to
 *	-The value being looked up is the node of the AST the variable
 *	 is associated with
 * */




//Functionality needed for part 1
//	1) IF FUNCTION: Identify whether a function already exists in the env
//	   IF STRUCT/ENUM: Identify whether a struct/enum already exists in the env
//
//	2) Field/Parameter Check: Check whether a field or parameter has been declared
//	   			  in definition already



struct type* lookup_var(struct env * env , GNode* node){

	char* id;
	unsigned int qid;

	if( get_ast(node)->kind == ID ){	
		id = get_ast( node )->str;
		qid = g_quark_from_string( id  );	
	}
	else{//FNCALL...
		id = get_ast( node->children )->str;
		qid = g_quark_from_string( id  );	
	}


	return (struct type*) g_hash_table_lookup( env->vars , GINT_TO_POINTER(qid) );
			
}

//STRUCT or ENUM
struct type * lookup_type(struct env * env , GNode* node){

	char* id;
	unsigned int qid;	


	switch(get_ast(node)->kind){
		case ENUM_LIT:
			id = get_ast( node->children->children )->str;		
			qid = g_quark_from_string( id );
			break;
		case ID:
			id = get_ast(node)->str;
			qid = g_quark_from_string( id  );
			break;
		default:
			id = get_ast( node->children)->str;		
			qid = g_quark_from_string( id );
	}


	return (struct type *) g_hash_table_lookup( env->types , GINT_TO_POINTER(qid) );
			
}



void insert_var(struct env * env, GNode* node){
	
	if(!node || !env)
		return;	
	
	struct ast* id_data = get_ast(node);
	char* id;
	struct type* record;	
	unsigned int qid;

	if(id_data->kind == ID){
		id = id_data->str;
		record = get_type( node );
		qid = g_quark_from_string( id );	
	}
	else if(id_data->kind == FNDEF_ITEM){
		id = get_ast( node->children )->str;
		record = get_type(  get_fndef_return(node)   );
		qid = g_quark_from_string( id );	
	}			

	g_hash_table_insert( env->vars , GINT_TO_POINTER(qid) , (gpointer)record ) ;

}

void insert_type(struct env* env, GNode * node){


	char * id = get_ast( node->children )->str;
	struct type* record = get_type(node);
	record->id = id;
	if(get_ast(node)->kind == ENUMDEF_ITEM){
		record->params = node;//shallow copy of the node! (will be used to get ctor hash table)

	}
	else{
		record->params = node->children->next->children;	
	}
	unsigned int qid = g_quark_from_string( id );	
	g_hash_table_insert( env->types , GINT_TO_POINTER(qid) , (gpointer)record ) ;

}



int main_exists = 0;

struct env * survey_defs(GNode* crate){

	GNode* item = crate->children->children; // (crate) -> (items) -> (item - item - item ... )
	
	struct env* def_env = init_env();
	struct ast* data;
	//int correct_main = 0;

	while(item){
	
		data = (struct ast*)(item->data);			
		//check if variable exists
		if( lookup_type(def_env, item) || lookup_var(def_env, item) ){
			
			if(data->kind == FNDEF_ITEM)
				;//printf("Error: duplicate definition of function `%s`.", get_item_id(item) );
			else
				;//printf("Error: duplicate definition of struct or enum `%s`.", get_item_id(item) );

		}
		else if ( data->kind == FNDEF_ITEM){
			//TYPE ANNOTATE RETURN STATEMENT

			
			GNode * ret = get_fndef_return( item );
			mark_type( ret , type_annotate_complex_type(ret->children , def_env)  );
			get_type(ret)->params = item ;		        	

			//insert item into environment
			insert_var(def_env, item);
			//flag to mark that a valid return exists in the function
		}
		else{
			get_type(item)->kind = TYPE_OK;
			insert_type(def_env, item);
		}
		
		//Check item's field right here
		//typecheck_fn_params(item);
		
			
		survey_item_fields(item, def_env);
		
		item = item->next;
	}

	if(main_exists == 0) { }//printf("Error: main function not found.");};

	//GNode * rec = g_hash_table_lookup(def_env,);
return def_env;
}



void survey_item_fields(GNode* item , struct env* global){

	struct ast* item_ast = (struct ast*)(item->data);	
	
	switch(item_ast->kind){

		case FNDEF_ITEM:
			if(!strcmp(get_item_id(item),"main")) {
				survey_main_fn(item);
				main_exists = 1;
			}
			else{
				survey_fn_params(item, global);
			}
			break;
		case STRUCTDEF_ITEM:
			survey_struct_fields(item , global);
			break;
		case ENUMDEF_ITEM:
			survey_enum_ctors(item , global);
			break;	
	}	
	
	
}



//Three functions
//	Enum ctors
//			
//	Struct fields
//	
//	Function params
//	


void survey_fn_params(GNode* fn_def, struct env* global){
	
	if(!fn_def)
		return;


	struct ast* fn_ast = get_ast(fn_def);

	
	//if(fn_ast->fields == NULL)
	fn_ast->fields = g_hash_table_new( NULL , NULL );

	//GHashTable* fn_params_table = fn_ast->fields;
	GNode* fn_param = get_fndef_params(fn_def);


	char* id;	
	while(fn_param){
		
		id = get_param_id(fn_param);
		GQuark qid = g_quark_from_string(id);	
		
		//VERIFY VARIABLE HAS UNIQUE IDENTIFIER

		//If the variable already exists	
		if( g_hash_table_lookup( fn_ast->fields , GINT_TO_POINTER(qid) ) ){
			;//printf("Error: identifier `%s` is bound more than once in the parameter list for the function `%s`.\n", id,
												//get_item_id(fn_def) );
		}
		//Insert into parameters hash table
		else{
			//GIVE THE FUNCTION PARAMETER A TYPE  fn_param -> id -> type
			struct type * tmp =  type_annotate_complex_type( fn_param->children->next, global );
			if(tmp->kind == TYPE_ERROR) get_type(fn_def)->kind = TYPE_ERROR;
			mark_type(  fn_param  ,	 tmp  );
			g_hash_table_insert( fn_ast->fields , GINT_TO_POINTER(qid) , (gpointer)get_type( fn_param ) );
		}
		

	
		fn_param = fn_param->next;
	}
	
	
}

void survey_main_fn(GNode * fn_main){
		GNode * fn_ret = (g_node_last_child(fn_main)->prev);
		
		get_ast(fn_main)->fields = g_hash_table_new(NULL, NULL);		
	
		if(get_ast(fn_ret)->type->kind != TYPE_UNIT){
			//printf("Error: main function has the wrong type.\n");
		}
}

void survey_struct_fields(GNode* struct_def, struct env *global){

	struct ast* struct_ast = get_ast(struct_def);
		
		
	//if(struct_ast->fields == NULL){
		struct_ast->fields = g_hash_table_new( NULL , NULL );
	//}

	GHashTable* struct_fields_table = struct_ast->fields;
	GNode* struct_field = struct_def->children->next->children; // (struct_def)->(id)-(struct_fields)->(field - field ...)

	//Will be useful for field lookups. Quick access to fields table
	get_ast(struct_field)->fields = struct_fields_table;

	char* id;	

	while(struct_field){

		id = get_field_id(struct_field);
		GQuark qid = g_quark_from_string(id);		
	
		//VERIFY EACH FIELD IS UNIQUE		
		if( g_hash_table_lookup( struct_fields_table , GINT_TO_POINTER(qid) ) ){
			//printf("Error: duplicate declaration of the field `%s` of the struct `%s`.\n", id,
				//get_item_id(struct_def) );
		}
		else{
			//GET TYPE OF FIELD	
			struct type * tmp =  type_annotate_complex_type( struct_field->children->next, global );
			if(tmp->kind == TYPE_ERROR){  get_type(struct_def)->kind = TYPE_ERROR;  }
			mark_type(  struct_field  ,	tmp  );
					
			//INSERT TYPE INTO STRUCT'S FIELDS TABLE
			g_hash_table_insert( struct_fields_table , GINT_TO_POINTER(qid) , (gpointer)tmp );
		}

				struct_field = struct_field->next;
	}
	
		
}
void survey_enum_ctors(GNode * parent , struct env* global){

	struct ast* cdata;
	struct ast* pdata;
	pdata = (struct ast *)(parent->data);	

	pdata->fields = g_hash_table_new(NULL, NULL);
	GNode * ctor = parent->children->next->children;
	GNode * child;
		
	while(ctor){
		child = ctor->children;
		cdata = (struct ast *)(child->data);
		unsigned int id =  g_quark_from_string( cdata->str );
		if(child->next == NULL){
		   if(!g_hash_table_insert( pdata->fields , GINT_TO_POINTER(id) , (gpointer)ctor )){
		   //printf("Error: duplicate definition of the constructor `%s` for the enum `%s`.\n"
								//, cdata->str, get_item_id(parent));	
			 };

		}
		else{
		   if(!g_hash_table_insert( pdata->fields , GINT_TO_POINTER(id) , (gpointer)ctor )){
		   //printf("Error: duplicate definition of the constructor `%s` for the enum `%s`.\n"
								//, cdata->str, get_item_id(parent));	
			 };

			GNode * argtype = child->next->children;
			while(argtype){//annotate the ctor argument types
				get_ast(argtype)->type = type_annotate_complex_type(argtype , global);
				if(get_type(argtype)->kind == TYPE_ERROR ){
					get_type(ctor)->kind = TYPE_ERROR;
					get_type(parent)->kind = TYPE_ERROR;
				} 
				argtype = argtype->next;
			}
		
		}

		ctor = ctor->next;
	}
}

char* get_item_id(GNode* item){
	
	struct ast* ast_ptr = (struct ast*)(item->children->data);
	return ast_ptr->str;	

}

char* get_param_id(GNode *fn_param){

	struct ast* ast_ptr = (struct ast*)(fn_param->children->children->data);//Assuming can only be pat-id
	return ast_ptr->str;

}

char* get_field_id(GNode* struct_field){

	struct ast* ast_ptr = (struct ast*)(struct_field->children->data);
	return ast_ptr->str;
}


struct env* init_env(){

	struct env* new_env = (struct env*)malloc( sizeof(struct env) );
	
	new_env->types = g_hash_table_new(NULL, NULL);
	new_env->vars = g_hash_table_new(NULL, NULL);
	
	return new_env;

}


//Returns a GNode pointer to the first parameter 
//Returns NULL if no parameters
GNode* get_fndef_params(GNode* fndef){

        GNode* child = fndef->children;


        while(  child   &&   get_ast(child)->kind != FNDEFPARAMS_ITEM  ){
                child = child->next;
        }
	if(!child)
		return NULL;

        return child->children;
}

GNode* get_fndef_return(GNode* fndef){

        GNode* child = fndef->children;

        while(  child   &&   get_ast(child)->kind != FNDEFRET_ITEM  ){
                child = child->next;
        }
        return child;

}




