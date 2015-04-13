#ifndef RUSTC_TYPE_H_
#define RUSTC_TYPE_H_

#include <stdbool.h>
#include <glib.h>
#include "symbol.h"

enum {
      TYPE_INVALID,
      TYPE_ERROR,
      TYPE_OK,
      TYPE_UNIT,
      TYPE_I32,
      TYPE_U8,
      TYPE_BOOL,
      TYPE_DIV,
      TYPE_REF,
      TYPE_REF_MUT,
      TYPE_SLICE,
      TYPE_ARRAY,
      TYPE_BOX,
      TYPE_ID,
      TYPE_FN,
};

struct type {
      int kind;
      bool mut;
      GList* params;
      struct type* type;
      int length;
      Symbol id;
};

/* Constructors. */
struct type* type_invalid(void);
struct type* type_error(void);
struct type* type_ok(void);
struct type* type_unit(void);
struct type* type_i32(void);
struct type* type_u8(void);
struct type* type_bool(void);
struct type* type_div(void);
struct type* type_ref(struct type* type);
struct type* type_ref_mut(struct type* type);
struct type* type_slice(struct type* type);
struct type* type_array(struct type* type, int length);
struct type* type_box(struct type* type);
struct type* type_id(Symbol id);
struct type* type_fn(GList* params, struct type* ret);

/* Create (allocate) a new type as a copy of the parameter type. */
struct type* type_copy(struct type* type);

/* Recursively free memory. */
void type_destroy(struct type* type);

#endif
