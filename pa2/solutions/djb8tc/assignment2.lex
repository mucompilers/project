%option noyywrap
%s IN_COMMENT
%{
#include <stdio.h>
#include <string.h>
#include <glib.h>
#include "assignment2.tab.h"
void yyerror(char const *s);
int yyparse(void);

/* handle locations */
int yycolumn = 1;

#define YY_USER_ACTION yylloc.first_line = yylloc.last_line = yylineno; \
    yylloc.first_column = yycolumn; yylloc.last_column = yycolumn+yyleng-1; \
    yycolumn += yyleng;
%}
%option yylineno
%%	
<INITIAL>{
"/*"              BEGIN(IN_COMMENT);
	/*WHITESPACE*/
[ \t] {  }
	/*COMMENTS*/
\/\/.*\n { ++yylineno; yycolumn = 1; }
	/*TOKENS USED*/
fn { return FN; }
:: {return DOUBLECOLON;}
&& {return AND;}
\|\| {return OR;}
\+\= {return ASSIGNADD;}
\-\= {return ASSIGNSUB;}
\*\= {return ASSIGNMUL;}
\/\= {return ASSIGNDIV;}
%\= {return ASSIGNREM;}
\=\= {return EQ;}
!\= {return NEQ;}
\<\= {return LEQ;}
\>\= {return GEQ;}
\( {return '(';}
\) {return ')';}
\- {return '-';}
\> {return '>';}
\< {return '<';}
! {return '!';}
\+ {return '+';}
\* {return '*';}
\/ {return '/';}
% {return '%';}
& {return '&';}
, {return ',';}
\{ {return '{';}
\} {return '}';}
: {return ':';}
\[ {return '[';}
\] {return ']';}
\. {return '.';}
\= {return '=';}
\| {return '|';}
; {return ';';}
_ {return '_';}
enum {return ENUM;}
struct {return STRUCT;}
mut {return MUT;}
ref {return REF;}
i32 {return I32;}
u8 {return U8;}
bool {return BOOL;}
Box {return BOX;}
let {return LET;}
return {return RET;}
true {return T;}
false {return F;}
while {return WHILE;}
loop {return LOOP;}
if {return IF;}
else {return ELSE;}
match {return MATCH;}
new {return NEW;}
	/*ID*/
[a-zA-Z_][a-zA-Z_0-9]* { yylval.string = strdup(yytext); return ID; }
	/*LITCHAR*/
b\'(\\\\|\\\'|[^\\\'])\' { return LITCHAR; }
	/*LITSTR*/
b\"(\\\\|\\\"|[^\\\"\n])+\" { return LITSTR; }
	/*LITDEC*/
[0-9][0-9_]* { yylval.num = atoi(yytext); return LITDEC; }
	/*ERRORS*/
. { printf("ERROR! Line %d: Invalid Token: %s\n", yylineno, yytext); }
	/*NEW LINE*/
\n { ++yylineno; yycolumn = 1; }
}
<IN_COMMENT>{
"*/"      BEGIN(INITIAL);
[^*\n]+   
"*"       
\n        { yylineno++; yycolumn = 1; }
}
%%