#include <assert.h>
#include <stdlib.h>
#include "type.h"
#include "ast.h" // For pair (params).

static struct type* type_new(int kind) {
      struct type* n = calloc(1, sizeof(*n));
      assert(n);
      n->kind = kind;
      return n;
}
struct type* type_invalid(void) {
      static struct type n = {.kind = TYPE_INVALID};
      return &n;
}
struct type* type_error(void) {
      static struct type n = {.kind = TYPE_ERROR};
      return &n;
}
struct type* type_ok(void) {
      static struct type n = {.kind = TYPE_OK};
      return &n;
}
struct type* type_unit(void) {
      static struct type n = {.kind = TYPE_UNIT};
      return &n;
}
struct type* type_i32(void) {
      static struct type n = {.kind = TYPE_I32};
      return &n;
}
struct type* type_u8(void) {
      static struct type n = {.kind = TYPE_U8};
      return &n;
}
struct type* type_bool(void) {
      static struct type n = {.kind = TYPE_BOOL};
      return &n;
}
struct type* type_div(void) {
      static struct type n = {.kind = TYPE_DIV};
      return &n;
}
struct type* type_ref(struct type* type) {
      struct type* n = type_new(TYPE_REF);
      n->type = type;
      return n;
}
struct type* type_mut(struct type* type) {
      struct type* n = type_new(TYPE_MUT);
      n->type = type;
      return n;
}
struct type* type_ref_mut(struct type* type) {
      return type_ref(type_mut(type));
}
struct type* type_slice(struct type* type) {
      struct type* n = type_new(TYPE_SLICE);
      n->type = type;
      return n;
}
struct type* type_array(struct type* type, int length) {
      struct type* n = type_new(TYPE_ARRAY);
      n->type = type;
      n->length = length;
      return n;
}
struct type* type_box(struct type* type) {
      struct type* n = type_new(TYPE_BOX);
      n->type = type;
      return n;
}
struct type* type_id(Symbol id) {
      struct type* n = type_new(TYPE_ID);
      n->id = id;
      return n;
}
struct type* type_fn(GList* params, struct type* ret) {
      struct type* n = type_new(TYPE_FN);
      n->params = params;
      n->type = ret;
      return n;
}

bool type_eq_mod_mut(const struct type* left, const struct type* right) {
      assert(left); assert(right);

      if (left->kind == TYPE_MUT) left = left->type;
      if (right->kind == TYPE_MUT) right = right->type;

      return type_eq(left, right);
}

bool type_eq(const struct type* left, const struct type* right) {
      if (left == right) return true;

      assert(left && right);

      if (left->kind != right->kind) return false;

      switch (left->kind) {
            case TYPE_INVALID:
            case TYPE_ERROR:
            case TYPE_OK:
            case TYPE_UNIT:
            case TYPE_I32:
            case TYPE_U8:
            case TYPE_BOOL:
            case TYPE_DIV:
                  return false;

            case TYPE_REF:
            case TYPE_MUT:
            case TYPE_SLICE:
            case TYPE_ARRAY:
            case TYPE_BOX:
                  return type_eq(left->type, right->type);

            case TYPE_FN: {
                  // TODO (though probably shouldn't need this).
                  assert(false);
            }

            default: assert(false);
      }

}

bool type_is_bool(struct type* type) {
      return type_eq_mod_mut(type, type_bool());
}

bool type_is_i32(struct type* type) {
      return type_eq_mod_mut(type, type_i32());
}

bool type_is_unit(struct type* type) {
      return type_eq_mod_mut(type, type_unit());
}

bool type_is_array(struct type* type) {
      assert(type);
      return type->kind == TYPE_ARRAY
            || (type->kind == TYPE_MUT && type->type->kind == TYPE_ARRAY);
}

struct type* type_get_elem(struct type* type) {
      assert(type_is_array(type));
      if (type->kind == TYPE_ARRAY) {
            return type->type;
      } else {
            assert(type->type);
            return type->type->type;
      }
}

struct type* type_copy(const struct type* old) {
      assert(old);

      switch (old->kind) {
            case TYPE_INVALID:
            case TYPE_ERROR:
            case TYPE_OK:
            case TYPE_UNIT:
            case TYPE_I32:
            case TYPE_U8:
            case TYPE_BOOL:
            case TYPE_DIV:
                  return old;

            case TYPE_REF:
            case TYPE_MUT:
            case TYPE_SLICE:
            case TYPE_ARRAY:
            case TYPE_BOX: {
                  struct type* new = calloc(1, sizeof(*new));
                  *new = *old;
                  new->type = type_copy(old->type);
                  return new;
            }

            case TYPE_FN: {
                  struct type* new = calloc(1, sizeof(*new));
                  *new = *old;

                  // TODO (though probably shouldn't need this).
                  assert(false); // need to copy params.

                  return new;
            }

            default: assert(false);
      }
}

void type_destroy(struct type* type) {
      if (!type) return;

      switch (type->kind) {
            case TYPE_INVALID:
            case TYPE_ERROR:
            case TYPE_OK:
            case TYPE_UNIT:
            case TYPE_I32:
            case TYPE_U8:
            case TYPE_BOOL:
            case TYPE_DIV:
                  return;

            case TYPE_REF:
            case TYPE_MUT:
            case TYPE_SLICE:
            case TYPE_ARRAY:
            case TYPE_BOX:
                  type_destroy(type->type);
                  break;

            case TYPE_FN:
                  type_destroy(type->type);
                  g_list_free_full(type->params, (GDestroyNotify)pair_destroy);
                  break;
      }

      free(type);
}
