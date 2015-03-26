%{

#include "parser.h"
#include <string.h>

%}



%option noyywrap
%option nounput
%option noinput
%option yylineno

%%

\/\*([^*]|\*[^/])*\*+\/ { /* MULTILINE COMMENTS */ }
"//"(.)*[\n\r]   { /* INLINE COMMENTS */ }
b[\"].*[\"] { return BYTE_STR; }
b[\'].[\']   { return BYTE_CHAR; }
[\"][a-zA-Z_][_a-zA-Z0-9]+[\"]   { return LIT_STR; }
[\'][a-zA-Z][\']   { return LIT_CHAR; }
Box         { return BOX; }
Box::new    { return BOX_NEW; }
"\&\&"      { return AND; }
\&          { return ADDR_OF; }
mut         { return MUT; }
if          { return IF; }
else        { return ELSE; }
loop        { return LOOP; }
while       { return WHILE; }
fn          { return FN; }
enum        { return ENUM; }
struct      { return STRUCT; }
match       { return MATCH; }
let         { return LET; }
return      { return RETURN; }
bool        { return TYPE_BOOL; }
true        { return BOOL_TRUE; }
false       { return BOOL_FALSE; }
i32         { return TYPE_I32; }
u8          { return TYPE_U8; }
ref         { return REF; }
"\|\|"      { return OP_OR; }
"<\="          { return LEQ; }
">="          { return GEQ; }
"\<"           { return LT; }
\>           { return GT; }
"%"           { return REM; }
\+\=        {return ASSIGN_ADD;}
!\=          {return NEQ; }
\=>          { return ASSIGN_PAT; };
\=\=        {return ASSIGN;}
\*\=        {return ASSIGN_MUL;}
\-\=        {return ASSIGN_SUB;}
\/\=        {return ASSIGN_DIV;}
"%="        {return ASSIGN_REM;}
\|          {return OR;}
!           { return NOT; }
->          { return FN_RETURN_TYPE; }
:           { return ':'; }
;           { return ';'; }
\=          { return EQ; }
\,           { return ','; }
\[          { return '['; }
\]          { return ']'; }
\(          { return '('; }
\)          { return ')'; }
\{          { return '{'; }
\}          { return '}'; }
\+          { return ADD; }
\_          { return WILD; }
-          { return SUB; }
\*          { return MUL; }
"\/"          { return DIV; }
\.          { return '.'; }
[0-9]+      { return LIT_DEC; }
[a-zA-Z_][_a-zA-Z0-9]*  { yylval.value = strdup(yytext); return ID; }
[ \n\r\t]+  { /* Eat up whitespace. */ }
.           { return UNKNOWN; }

%%
