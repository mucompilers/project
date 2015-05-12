#ifndef	ENV_H
#define ENV_H	

#include <glib.h>

struct env{
	struct env *next;
	struct env *prev;
	
	GHashTable *fn_def;
	GHashTable *se_def;

};

/*look up a definition in the environment*/
GNode* 	lookup_def(struct env*, GNode*);

/*insert a definition into the environment*/
void 	insert_def(struct env*, GNode*);

/*typecheck the all definitions in a crate*/
void  	typecheck_defs(GNode* crate);

void	typecheck_item_fields(GNode*);

/*get the identifier for an item/definition */
char* 	get_item_id(GNode* id);

/*initialize environment*/
struct env* init_def_env();

void typecheck_main_fn(GNode *);

/*Typecheck function definition parameters*/
void typecheck_fn_params(GNode*);

/*Get the id of a function definition parameter*/
char* get_param_id(GNode*);

/**/
void typecheck_struct_fields(GNode*);

void typecheck_enum_ctors(GNode*);



/**/
char* get_field_id(GNode*);

#endif /*ENV_H*/
