#ifndef ENV_H_
#define ENV_H_

#include <glib.h>
#include <stdbool.h>
#include "ast.h"

struct type{
    int kind;
    GList * params;
    struct type* type;
    int length;
    char* id;
};

struct env{
    GHashTable *vars;
    GHashTable *types;
};


enum{
   TYPE_INVALID,//0
   TYPE_ERROR,//1
   TYPE_OK,//2
   TYPE_UNIT,//3
   TYPE_I32,//4
   TYPE_U8,
   TYPE_BOOL,
   TYPE_DIV,
   TYPE_REF,
   TYPE_MUT,
   TYPE_SLICE,//10
   TYPE_ARRAY,//11
   TYPE_BOX,//12
   TYPE_ID,
   TYPE_ARRAY_ID,//14
   
   TYPE_STRUCT,//15
   TYPE_FN,//16
   TYPE_ENUM,
};
void type_print(struct ast* );
void single_type_print(struct type*);

void ast_calc_type(struct ast* ,struct env* );
void ast_calc_var(struct ast*, struct env* );
bool judge_is_var(struct ast*);

bool type_is_ok(struct type*);
bool type_is_unit(struct type*);
bool type_is_i32(struct type*);
bool type_is_invalid(struct type*);
bool type_is_mut(struct type*);
bool type_is_error(struct type*);
bool type_is_bool(struct type*);


struct type* type_new(int );
struct type* type_mut(struct type*);
struct type* type_bool();
struct type* type_ok();
struct type* type_error();
struct type* type_i32();
struct type* type_invalid();
struct type* type_unit();
struct type* type_mut();
struct type* type_u8();
struct type* type_div();
struct type* type_ref(struct type*);
struct type* type_array(struct type*, int);
struct type* type_slice(struct type*, char*);
struct type* type_item(GList*, int);
struct type* type_fn(GList*, int, struct type*);
struct type* type_box(struct type*);

void type_destroy(struct type* );
bool type_eq(const struct type* left, const struct type* right);

struct env* env_new();
void var_insert(char*, struct type*, struct env*);
void type_insert(char*, struct type*, struct env*);
static void insert_into(char*, struct type*, GHashTable*);
struct type* env_lookup_type(struct env* , char*);
struct type* env_lookup_var(struct env* , char*);
struct env* env_copy(struct env*);
struct type* env_lookup(struct env*, char *);
void env_free(struct env*);
static void insert_copy_into(char*, struct type*, GHashTable*);

static struct type* strip_mut(const struct type*);

struct type* struct_find_type(struct type*, char*);



#endif
