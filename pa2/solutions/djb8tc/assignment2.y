%{
#include <stdio.h>
#include <glib.h>
#include <string.h>

int yylex(void);
void yyerror(char const *s);

typedef struct ast {
	char* value;
	GList* children;
	struct ast* parent;
} AST;

AST* create_node(char* v, GList* c);
void print_tree(AST* head, int tabcount);
void free_tree(AST* head);

AST* head = NULL;

%}

%expect 68

%define parse.error verbose

%locations

%code requires {
#include <glib.h>
}

%union {
	int num;
	char* string;
	char c;
	struct ast* ast;
	GList* list;
}

%token <string> ID
%token <c> LITCHAR
%token <string> LITSTR
%token <num> LITDEC
%token <string> FN
%token <string> DOUBLECOLON
%token <string> ENUM
%token <string> STRUCT
%token <string> REF
%token <string> MUT
%token <string> I32
%token <string> U8
%token <string> BOOL
%token <string> BOX
%token <string> LET
%token <string> RET
%token <string> T
%token <string> F
%token <string> WHILE
%token <string> LOOP
%token <string> IF
%token <string> ELSE
%token <string> MATCH
%token <string> NEW
%left '=' ASSIGNADD ASSIGNSUB ASSIGNMUL ASSIGNDIV ASSIGNREM
%left OR
%left AND
%left EQ NEQ '<' '>' LEQ GEQ
%left '+' '-'
%left '*' '/' '%'
%left '&' '!' NEG DEREF

%type<ast> crate items fn-def fn-params fn-param enum-def enum-ctor-defs enum-ctor-def enum-ctor-params struct-def field-defs field-def types type-ref type-ref-mut type-arr type-i32 type-u8 type-bool type-box type-unit block stmt let return pats pat-lit pat-id pat-ref-id pat-mut-id pat-ref-mut-id pat-deref pat-arr pat-arr-elems pat-enum pat-enum-ctor-params pat-struct pat-fields pat-field pat-unit pat-wild exprs id enum enum-ctor struct field-init arr lit-dec true false lit-str lit-char unit add sub mul div rem neg and or not lt gt leq geq eq neq addr-of deref addr-of-mut field-lookup arr-index assign assign-add assign-sub assign-mul assign-div assign-rem while loop if fn-call box-new type-id match match-arm
%type<list> item-list fn-param-list enum-ctor-def-list enum-ctor-param-list field-def-list stmts pat-list pat-fields-list exprs-list field-inits arr-elem-list match-arms match-arm-pats

%start crate

%%

crate: items { head = create_node("crate", g_list_append(NULL, $1)); print_tree(head, 0); printf("\n"); /*free_tree(head);*/}

items: item-list { $$ = create_node("items", $1); }

item-list: item-list fn-def  { $$ = g_list_append($1, $2); }
   | item-list enum-def { $$ = g_list_append($1, $2); }
   | item-list struct-def { $$ = g_list_append($1, $2); }
   | fn-def { $$ = g_list_append(NULL, $1); }
   | enum-def { $$ = g_list_append(NULL, $1); }
   | struct-def { $$ = g_list_append(NULL, $1); }
   
fn-def: FN id '(' fn-params ')' block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); children = g_list_append(children, $6); $$ = create_node("fn-def", children); }
   | FN id '(' fn-params ')' '-' '>' types block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); children = g_list_append(children, $8); children = g_list_append(children, $9); $$ = create_node("fn-def", children); }
   | FN id '(' fn-params ')' '-' '>' '!' block { GList* children = NULL; children = g_list_append(children, $2);  children = g_list_append(children, $4); children = g_list_append(children, $9); $$ = create_node("fn-def", children); }
   | FN id '(' ')' block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $5); $$ = create_node("fn-def", children); }
   | FN id '(' ')' '-' '>' types block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $7); children = g_list_append(children, $8); $$ = create_node("fn-def", children); }
   | FN id '(' ')' '-' '>' '!' block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $8); $$ = create_node("fn-def", children); }

fn-params: fn-param-list { $$ = create_node("fn-params", $1); }
   
fn-param-list: fn-param { $$ = g_list_append(NULL, $1); }
   | fn-param-list ',' fn-param { $$ = g_list_append($1, $3); }
   
fn-param: pats ':' types { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("fn-param", children); }

enum-def: ENUM id '{' enum-ctor-defs '}' { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); $$ = create_node("enum-def", children); }

enum-ctor-defs: enum-ctor-def-list { $$ = create_node("enum-ctor-defs", $1); }
   
enum-ctor-def-list: enum-ctor-def { $$ = g_list_append(NULL, $1); }
   | enum-ctor-def-list ',' enum-ctor-def { $$ = g_list_append($1, $3); }

enum-ctor-def: id { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("enum-ctor-def", children); }
   | id '(' enum-ctor-params ')' { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("enum-ctor-def", children); }

enum-ctor-params: enum-ctor-param-list { $$ = create_node("enum-ctor-params", $1); }

enum-ctor-param-list: types { $$ = g_list_append(NULL, $1); }
   | enum-ctor-param-list ',' types { $$ = g_list_append($1, $3); }
   
struct-def: STRUCT id '{' field-defs '}' { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); $$ = create_node("struct-def", children); }

field-defs: field-def-list { $$ = create_node("field-defs", $1); }
   
field-def-list: field-def { $$ = g_list_append(NULL, $1); }
   | field-def-list ',' field-def { GList* list = g_list_append($1, $3); }
   
field-def: id ':' types { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("field-def", children); }

types: type-ref { $$ = $1; }
   | type-ref-mut { $$ = $1; }
   | type-arr { $$ = $1; }
   | type-i32 { $$ = $1; }
   | type-u8 { $$ = $1; }
   | type-bool { $$ = $1; }
   | type-box { $$ = $1; }
   | type-unit { $$ = $1; }
   | type-id { $$ = $1; }

type-ref: '&' types { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("type-ref", children); } 

type-ref-mut: '&' MUT types { GList* children = NULL; children = g_list_append(children, $3); $$ = create_node("type-ref-mut", children); } 

type-arr: '[' types ']' { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("type-arr", children); }
   | '[' types ';' lit-dec ']' { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); $$ = create_node("type-arr", children); }

type-i32: I32 { $$ = create_node("type-i32", NULL); }

type-u8: U8 { $$ = create_node("type-u8", NULL); }

type-bool: BOOL { $$ = create_node("type-bool", NULL); }

type-box: BOX '<' types '>' { GList* children = NULL; children = g_list_append(children, $3); $$ = create_node("type-box", children); }

type-unit: '(' ')' { $$ = create_node("type-unit", NULL); }

type-id: ID { GList* children = NULL; AST* value = create_node(yylval.string, NULL); free(yylval.string); children = g_list_append(children, value); $$ = create_node("id", children); }

block: '{' stmts '}' { $$ = create_node("block", $2); }
   | '{' stmts exprs '}' { GList* children = $2; children = g_list_append(children, $3); $$ = create_node("block", children); }

stmts: { $$ = NULL; }
   | stmts stmt { $$ = g_list_append($1, $2); }
   
stmt: let ';' { $$ = $1; }
   | return ';' { $$ = $1; }
   | exprs ';' { $$ = $1; }
   
let: LET pats {GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("let", children); }
   | LET pats ':' types {GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); $$ = create_node("let", children); }
   | LET pats '=' exprs {GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); $$ = create_node("let", children); }
   | LET pats ':' types '=' exprs {GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $4); children = g_list_append(children, $6); $$ = create_node("let", children); }
   
return: RET { $$ = create_node("return", NULL); }
   | RET exprs { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("return", children); }

pats: pat-lit { $$ = $1; }
   | pat-id { $$ = $1; }
   | pat-ref-id { $$ = $1; }
   | pat-mut-id { $$ = $1; }
   | pat-ref-mut-id { $$ = $1; }
   | pat-deref { $$ = $1; }
   | pat-arr { $$ = $1; }
   | pat-enum { $$ = $1; }
   | pat-struct { $$ = $1; }
   | pat-unit { $$ = $1; }
   | pat-wild { $$ = $1; }
   
pat-lit: lit-dec { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-lit", children); }
   | '-' lit-dec { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("pat-lit", children); }
   | true { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-lit", children); }
   | false { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-lit", children); }
   | lit-str { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-lit", children); }
   | lit-char { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-lit", children); }
   
pat-id: id { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-id", children); }

pat-ref-id: REF id { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("pat-ref-id", children); }

pat-mut-id: MUT id { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("pat-mut-id", children); }

pat-ref-mut-id: REF MUT id { GList* children = NULL; children = g_list_append(children, $3); $$ = create_node("pat-ref-mut-id", children); }

pat-deref: '&' pat-id { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("pat-deref", children); }

pat-arr: pat-arr-elems { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-arr", children); }

pat-arr-elems: '[' pat-list ']'{ $$ = create_node("pat-arr-elems", $2); }
   
pat-enum: enum-ctor { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("pat-enum", children); }
   | enum-ctor pat-enum-ctor-params { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $2); $$ = create_node("pat-enum", children); }

pat-enum-ctor-params: '(' pat-list ')' { $$ = create_node("pat-enum-ctor-params", $2); }
   
pat-struct: id '{' pat-fields '}' { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("pat-struct", children); }

pat-fields: pat-fields-list { $$ = create_node("pat-fields", $1); }

pat-fields-list: pat-field { $$ = g_list_append(NULL, $1); }
   | pat-fields-list ',' pat-field { $$ = g_list_append($1, $3); }
   
pat-field: id ':' pats { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("pat-field", children); }

pat-unit: '(' ')' { $$ = create_node("pat-unit", NULL); }

pat-wild: '_' { $$ = create_node("pat-wild", NULL); }

pat-list: pats  { $$ = g_list_append(NULL, $1); }
   | pat-list ',' pats { $$ = g_list_append($1, $3); } 

exprs: id { $$ = $1; }
   | enum { $$ = $1; }
   | struct { $$ = $1; }
   | arr { $$ = $1; }
   | lit-dec { $$ = $1; }
   | true { $$ = $1; }
   | false { $$ = $1; }
   | lit-str { $$ = $1; }
   | lit-char { $$ = $1; }
   | unit { $$ = $1; }
   | field-lookup { $$ = $1; }
   | arr-index { $$ = $1; }
   | while { $$ = $1; }
   | loop { $$ = $1; }
   | if { $$ = $1; }
   | match { $$ = $1; }
   | fn-call { $$ = $1; }
   | box-new { $$ = $1; }
   | assign { $$ = $1; }
   | assign-add { $$ = $1; }
   | assign-sub { $$ = $1; }
   | assign-mul { $$ = $1; }
   | assign-div { $$ = $1; }
   | assign-rem { $$ = $1; }
   | or { $$ = $1; }
   | and { $$ = $1; }
   | lt { $$ = $1; }
   | gt { $$ = $1; }
   | leq { $$ = $1; }
   | geq { $$ = $1; }
   | eq { $$ = $1; }
   | neq { $$ = $1; }
   | add { $$ = $1; }
   | sub { $$ = $1; }
   | mul { $$ = $1; }
   | div { $$ = $1; }
   | rem { $$ = $1; }
   | neg { $$ = $1; }
   | addr-of { $$ = $1; }
   | addr-of-mut { $$ = $1; }
   | deref { $$ = $1; }
   | not { $$ = $1; }
   | '(' exprs ')' { $$ = $2; }
   
id: ID { GList* children = NULL; AST* value = create_node(yylval.string, NULL); free(yylval.string); children = g_list_append(children, value); $$ = create_node("id", children); }   

enum: enum-ctor '(' exprs-list ')' { AST* exprs = create_node("exprs", $3); GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, exprs); $$ = create_node("enum", children); }
   | enum-ctor { GList* children = NULL; children = g_list_append(children, $1); $$ = create_node("enum", children); }

exprs-list: exprs { $$ = g_list_append(NULL, $1); }
   | exprs-list ',' exprs { $$ = g_list_append($1, $3); }

enum-ctor: id DOUBLECOLON id { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("enum-ctor", children); }

struct: id '{' field-inits '}' { AST* field_inits = create_node("field-inits", $3); GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, field_inits); $$ = create_node("struct", children); }

field-inits: field-inits ',' field-init { $$ = g_list_append($1, $3); }
   | field-init { $$ = g_list_append(NULL, $1); }

field-init: id ':' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("field-init", children); }

arr: '[' arr-elem-list ']' { AST* exprs = create_node("exprs", $2); GList* children = NULL; children = g_list_append(children, exprs); $$ = create_node("arr", children); }

arr-elem-list: exprs { $$ = g_list_append(NULL, $1); }
   | arr-elem-list ',' exprs { $$ = g_list_append($1, $3); }

lit-dec: LITDEC { $$ = create_node("lit-dec", NULL); }

true: T { $$ = create_node("true", NULL); }

false: F { $$ = create_node("false", NULL); }

lit-str: LITSTR { $$ = create_node("lit-str", NULL); }

lit-char: LITCHAR { $$ = create_node("lit-char", NULL); }

unit: '(' ')' { $$ = create_node("unit", NULL); }

add: exprs '+' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("add", children); }

sub: exprs '-' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("sub", children); }

mul: exprs '*' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("mul", children); }

div: exprs '/' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("div", children); }

rem: exprs '%' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("rem", children); }

neg: '-' exprs %prec NEG { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("neg", children); }

and: exprs AND exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("and", children); }

or: exprs OR exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("or", children); }

not: '!' exprs { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("not", children); }

lt: exprs '<' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("lt", children); }

gt: exprs '>' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("gt", children); }

leq: exprs LEQ exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("leq", children); }

geq: exprs GEQ exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("geq", children); }

eq: exprs EQ exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("eq", children); }

neq: exprs NEQ exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("neq", children); }

addr-of: '&' exprs { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("addr-of", children); }

deref: '*' exprs %prec DEREF { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("deref", children); }

addr-of-mut: '&' MUT exprs { GList* children = NULL; children = g_list_append(children, $3); $$ = create_node("addr-of-mut", children); }

field-lookup: exprs '.' id { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("field-lookup", children); }

arr-index: exprs '[' exprs ']' { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("arr-index", children); }

assign: exprs '=' exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("assign", children); }

assign-add: exprs ASSIGNADD exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("assign-add", children); }

assign-sub: exprs ASSIGNSUB exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("assign-sub", children); }

assign-mul: exprs ASSIGNMUL exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("assign-mul", children); }

assign-div: exprs ASSIGNDIV exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("assign-div", children); }

assign-rem: exprs ASSIGNREM exprs { GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, $3); $$ = create_node("assign-rem", children); }

while: WHILE exprs block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $3); $$ = create_node("while", children); }

loop: LOOP block { GList* children = NULL; children = g_list_append(children, $2); $$ = create_node("loop", children); }

if: IF exprs block ELSE block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $3); children = g_list_append(children, $5); $$ = create_node("if", children); }
   | IF exprs block { GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, $3); $$ = create_node("if", children); }

match: MATCH exprs '{' match-arms '}' { AST* arms = create_node("match-arms", $4); GList* children = NULL; children = g_list_append(children, $2); children = g_list_append(children, arms); $$ = create_node("match", children); }

match-arms: match-arm { $$ = g_list_append(NULL, $1); }
   | match-arms ',' match-arm { $$ = g_list_append($1, $3); }

match-arm: match-arm-pats '=' '>' block { AST* pats = create_node("pats", $1); GList* children = NULL; children = g_list_append(children, pats); children = g_list_append(children, $4); $$ = create_node("match-arm", children); }

match-arm-pats: pats { $$ = g_list_append(NULL, $1); }
   | match-arm-pats '|' pats { $$ = g_list_append($1, $3); }
   
fn-call: id '(' exprs-list ')' { AST* exprs = create_node("exprs", $3); GList* children = NULL; children = g_list_append(children, $1); children = g_list_append(children, exprs); $$ = create_node("fn-call", children); }
   | id '(' ')' { GList* children = g_list_append(NULL, $1); AST* exprs = create_node("", NULL); children = g_list_append(children, exprs); $$ = create_node("fn-call", children); }

box-new: BOX DOUBLECOLON NEW '(' exprs ')' { GList* children = NULL; children = g_list_append(children, $5); AST* exprs = create_node("exprs", children); children = g_list_append(NULL, exprs); $$ = create_node("box-new", children); }

%%
AST* create_node(char* v, GList* c) {
	AST* a = (AST*)malloc(sizeof(AST));
	a->value = strdup(v);
	a->children = c;
	a->parent = NULL;
	AST* data;
	while (c != NULL) {
		data = c->data;
		data->parent = a;
		c = c->next;
	}
	return a;
}

void print_tree(AST* head, int tabcount) {
	int i;
	printf("\n");
	for (i = 0; i < tabcount; ++i) {
		printf("\t");
	}
	printf("(%s", head->value);
	++tabcount;
	GList* c;
	for (c = head->children; c != NULL; c = c->next) {
		print_tree(c->data, tabcount);
	}
	printf(")");
	return;
}

void free_tree(AST* head) {
	GList* c;
	AST* data;
	for (c = head->children; c != NULL; c = c->next) {
		data = c->data;
		free_tree(data);
	}
	free(head->value);
	free(head);
	return;
}

void yyerror(char const *s) {
	printf("Parsing error at lines %d, from columns %d to %d: %s\n", yylloc.first_line, yylloc.first_column, yylloc.last_column, s);
}