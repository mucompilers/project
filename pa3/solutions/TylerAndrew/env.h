#ifndef	ENV_H
#define ENV_H	

#include <glib.h>

struct env{
	struct env *next;
	struct env *prev;
	
	GHashTable *vars;
	GHashTable *types;

};



/*look up a definition in the environment*/
struct type* 	lookup_var(struct env*, GNode*);
struct type* 	lookup_type(struct env*, GNode*);

/*insert a definition into the environment*/
void 	insert_var(struct env*, GNode*);
void 	insert_type(struct env*, GNode*);
/*initial survey of all definitions in a crate*/
struct env * survey_defs(GNode* crate);

void	survey_item_fields(GNode* , struct env*);

/*get the identifier for an item/definition */
char* 	get_item_id(GNode* id);

/*initialize environment*/
struct env* init_env();

void survey_main_fn(GNode *);

/*Typecheck function definition parameters*/
void survey_fn_params(GNode*, struct env*);

/*Get the id of a function definition parameter*/
char* get_param_id(GNode*);

/**/
void survey_struct_fields(GNode*, struct env*);

void survey_enum_ctors(GNode*, struct env*);



/**/
char* get_field_id(GNode*);



GNode* get_fndef_params( GNode* );
GNode* get_fndef_return( GNode* );


#endif /*ENV_H*/
