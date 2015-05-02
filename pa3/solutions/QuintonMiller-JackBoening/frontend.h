#ifndef RUSTC_FRONTEND_H_
#define RUSTC_FRONTEND_H_
#include "ast.h"
#include "type.h"
#include <glib.h>

void parse_done(GList* items);
//int yyerror(char const* s);
struct type* get_type_crate(GList* items);
struct type* get_type_stmt(struct stmt* stmt, struct type* expected);
struct type* get_type_exp(struct exp* exp);
struct type* get_type_item(struct type* item);

#endif
