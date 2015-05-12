#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "env.h"
#include "ast.h"
#include "parser.h"

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



//Lookup a given function, struct, or enum definition in the environment.
//PARAMETERS:
//	def_env: the environment in which to look for the definition	
//	node: A pointer to the AST node of the definition that is being looked up
//RETURN:
//	A pointer to the node belonging to the looked up definition.
//      NULL if no record is found.
GNode* lookup_def(struct env* def_env , GNode *node){

	struct ast* def_data = (struct ast*)(node->data);
	struct ast* id_data = (struct ast*)(node->children->data);
	unsigned int id = g_quark_from_string( id_data->str );	
	GNode* rec;

	switch(def_data->kind){
	
		case FNDEF_ITEM:
			rec = (GNode*) g_hash_table_lookup( def_env->fn_def , GINT_TO_POINTER(id) );
			break;
		case STRUCTDEF_ITEM:
			rec = (GNode*) g_hash_table_lookup( def_env->se_def , GINT_TO_POINTER(id) );
			break;
		case ENUMDEF_ITEM:
			rec = (GNode*) g_hash_table_lookup( def_env->se_def , GINT_TO_POINTER(id) );
			break;
		default:
			break;

	}
	
	return rec;

}

void insert_def(struct env* def_env, GNode* node){

	struct ast* def_data = (struct ast*)(node->data);
	struct ast* id_data = (struct ast*)(node->children->data);
	unsigned int id = g_quark_from_string(id_data->str);	
		

	switch(def_data->kind){
	
		case FNDEF_ITEM:
			g_hash_table_insert( def_env->fn_def , GINT_TO_POINTER(id) , (gpointer)node ) ;
			break;
		case STRUCTDEF_ITEM:
			g_hash_table_insert( def_env->se_def , GINT_TO_POINTER(id) , (gpointer)node ) ;
			break;
		case ENUMDEF_ITEM:
			g_hash_table_insert( def_env->se_def , GINT_TO_POINTER(id) , (gpointer)node ) ;
			break;
		default:
			break;

	}

}

int main_exists = 0;
void typecheck_defs(GNode* crate){

	GNode* item = crate->children->children; // (crate) -> (items) -> (item - item - item ... )
	
	struct env* def_env = init_def_env();
	struct ast* data;
	//int correct_main = 0;

	while(item){
	
		data = (struct ast*)(item->data);			
		//check if variable exists
		if( lookup_def(def_env, item) ){
			
			if(data->kind == FNDEF_ITEM)
				printf("Error: duplicate definition of function `%s`.", get_item_id(item) );
			else
				printf("Error: duplicate definition of struct or enum `%s`.", get_item_id(item) );

		}
		else{	//If none exists insert into environment
			insert_def(def_env, item);
		}
		

		//Check item's field right here	
		typecheck_item_fields(item);
	
		item = item->next;
	}

	if(main_exists == 0) { printf("Error: main function not found.");};

	//GNode * rec = g_hash_table_lookup(def_env,);

}



void typecheck_item_fields(GNode* item){

	struct ast* item_ast = (struct ast*)(item->data);	
	
	switch(item_ast->kind){

		case FNDEF_ITEM:
			if(!strcmp(get_item_id(item),"main")) {	
				typecheck_main_fn(item);
				main_exists = 1;
			}
			else{
				typecheck_fn_params(item);
			}
			break;
		case STRUCTDEF_ITEM:
			typecheck_struct_fields(item);
			break;
		case ENUMDEF_ITEM:
			typecheck_enum_ctors(item);
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


void typecheck_fn_params(GNode* fn_def){

	struct ast* fn_ast = (struct ast*)(fn_def->data);

	if(fn_ast->fields == NULL)
		fn_ast->fields = g_hash_table_new( NULL , NULL );

	GHashTable* fn_params_table = fn_ast->fields;
	GNode* fn_param = fn_def->children->next->children; // (fn_def)->(id)-(fn_params)->(fn_param - fn_param ...)


	char* id;	
	while(fn_param){

		id = get_param_id(fn_param);
		GQuark qid = g_quark_from_string(id);		
	
	
		if( g_hash_table_lookup( fn_params_table , GINT_TO_POINTER(qid) ) ){
			printf("Error: identifier `%s` is bound more than once in the parameter list for the function `%s`.\n", id,
												get_item_id(fn_def) );
		}
		else{
			g_hash_table_insert(fn_params_table , GINT_TO_POINTER(qid) , (gpointer)fn_param );
		}

		fn_param = fn_param->next;
	}
	


}

void typecheck_main_fn(GNode * fn_main){
		struct ast * data = (struct ast *)(fn_main->children->next->data);
		if((data->kind != UNIT_TYPE) && (data->kind != BLOCK)){
			printf("Error: main function has the wrong type.\n");
		}
		//printf("Ok.\n");
}

void typecheck_struct_fields(GNode* struct_def){

	struct ast* struct_ast = (struct ast*)(struct_def->data);

	if(struct_ast->fields == NULL)
		struct_ast->fields = g_hash_table_new( NULL , NULL );

	GHashTable* struct_fields_table = struct_ast->fields;

	//Get to the pointer of fields
	GNode* struct_field = struct_def->children->next->children; // (struct_def)->(id)-(struct_fields)->(field - field ...)


	char* id;	

	while(struct_field){

		id = get_field_id(struct_field);
		GQuark qid = g_quark_from_string(id);		
	
	
		if( g_hash_table_lookup( struct_fields_table , GINT_TO_POINTER(qid) ) ){
			printf("Error: duplicate declaration of the field `%s` of the struct `%s`.\n", id,
												get_item_id(struct_def) );
		}
		else{
			g_hash_table_insert(struct_fields_table , GINT_TO_POINTER(qid) , (gpointer)struct_field );
		}

		struct_field = struct_field->next;
	}
	
		
}

void typecheck_enum_ctors(GNode * parent){

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
			printf("Error: duplicate definition of the constructor `%s` for the enum `%s`.\n", cdata->str, get_item_id(parent));	
			 };

		}
		else{
			if(!g_hash_table_insert( pdata->fields , GINT_TO_POINTER(id) , (gpointer)ctor )){
			printf("Error: duplicate definition of the constructor `%s` for the enum `%s`.\n", cdata->str, get_item_id(parent));	
			 };	
					
					//Gnode * typelist = child->next;
					//handle ctor arg type list with ctor ID in enviroment later
					//		

		}

		ctor = ctor->next;

	}

}

char* get_item_id(GNode* item){
	
	struct ast* ast_ptr = (struct ast*)(item->children->data);
	return ast_ptr->str;	

}

char* get_param_id(GNode *fn_param){

	struct ast* ast_ptr = (struct ast*)(fn_param->children->children->data);	//Assuming can only be pat-id
	return ast_ptr->str;

}

char* get_field_id(GNode* struct_field){

	struct ast* ast_ptr = (struct ast*)(struct_field->children->data);
	return ast_ptr->str;
}

struct env* init_def_env(){

	struct env* new_env = (struct env*)malloc( sizeof(struct env) );
	
	new_env->fn_def = g_hash_table_new(NULL, NULL);
	new_env->se_def = g_hash_table_new(NULL, NULL);
	
	return new_env;

}



