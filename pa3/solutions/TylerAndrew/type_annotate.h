#ifndef	TYPE_ANNOTATE_H
#define TYPE_ANNOTATE_H	

#include <glib.h>
#include "env.h"

enum {
	TYPE_INVALID = 700,
	TYPE_ERROR,
	TYPE_OK,
	TYPE_UNIT,
	TYPE_I32,
	TYPE_U8,
	TYPE_BOOL,
	TYPE_DIV,
	TYPE_REF,
	TYPE_MUT,
	TYPE_SLICE,
	TYPE_ARRAY,
	TYPE_BOX,
	TYPE_ID,
	TYPE_FN,
};

struct type{
	int kind;
	int length;
	char * id;
	GNode * params;
	struct type * type;
};


void type_annotate(GNode *, struct env *);

struct type * type_annotate_fndef(GNode *, struct env *, GHashTable *);

struct type * type_annotate_block(GNode *, struct env *, GHashTable *, struct type * ret_type);

struct type * type_annotate_exp_stmt(GNode *, struct env *, GHashTable *, struct type * ret_type);

struct type * type_annotate_binary_op_bool(GNode *, struct env *, GHashTable *);

struct type * type_annotate_binary_op_i32(GNode *, struct env *, GHashTable *);

struct type * type_annotate_equality(GNode *, struct env *, GHashTable *);

struct type * type_annotate_comparison(GNode *, struct env *, GHashTable *);

struct type * type_annotate_unary_op(GNode *, struct env *, GHashTable *);

struct type * type_annotate_if(GNode *, struct env *, GHashTable *, struct type * ret_type);

struct type * type_annotate_while(GNode *, struct env *, GHashTable *, struct type * ret_type);

struct type * type_annotate_loop(GNode *, struct env *, GHashTable *, struct type * ret_type);

struct type * type_annotate_arr_lit(GNode *, struct env *, GHashTable *);

struct type * type_annotate_enum_lit(GNode *, struct env *, GHashTable *);

struct type * type_annotate_struct_lit(GNode *, struct env *, GHashTable *);

struct type* type_annotate_fncall(GNode* , struct env* global, GHashTable *);

struct type* type_annotate_box_new(GNode* , struct env* global, GHashTable *);

struct type * type_annotate_deref(GNode *, struct env * , GHashTable * );

struct type * type_annotate_ref(GNode *, struct env * , GHashTable * );

struct type* type_annotate_id(GNode* , struct env* , GHashTable* );

struct type * type_annotate_arr_index(GNode *, struct env *, GHashTable *);

struct type* type_annotate_field_lookup(GNode *, struct env* , GHashTable *);

struct type * type_annotate_let_stmt(GNode *, struct env *, GHashTable *);

struct type * type_annotate_let_assign(GNode *, struct env *, GHashTable *);

struct type * type_annotate_let_type(GNode *, struct env *, GHashTable *);

struct type * type_annotate_let_type_assign(GNode *, struct env *, GHashTable *);

struct type * type_annotate_comp_assignment(GNode *, struct env *, GHashTable *);

struct type * type_annotate_assignment(GNode *, struct env *, GHashTable *);

/*TODO
struct type * type_annotate_arrayindex(GNode *, struct env *, GHashTable *);

struct type * type_annotate_flup(GNode *, struct env *, GHashTable *);

struct type * type_annotate_letstmt(GNode *, struct env *, GHashTable *);
*/

//Utility Section

void free_ast_node(struct ast* );
void free_ast(GNode* );
void free_nary(GNode* );
char* deepcopy(char*);
struct type* type_annotate_complex_type(GNode* , struct env*);
GNode* get_fncall_params(GNode* fncall);

struct type * strip_mut(struct type *);
struct type * mark_type(GNode *, struct type *);
int child_count(GNode*);
struct type * type_copy(struct type *);
int type_eq(struct type *, struct type *);
struct type * new_type(int, int, char * );
struct type * get_type(GNode *);
struct ast  * get_ast(GNode *);
int stc(struct type *, int);

void free_type(struct type*);

#endif //TYPE_ANNOTATE_H
