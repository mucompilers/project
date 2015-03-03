%{

#include "lexer.h"

%}

%option noyywrap
%option nounput
%option noinput

%%

"("          { return LPAREN; }
")"          { return RPAREN; }
"+"          { return ADD; }
"-"          { return SUB; }
"*"          { return MUL; }
"\\"         { return DIV; }
"^"          { return EXP; }
[\r\n]    { return 0; }
[0-9]+       { return NUM; }
[ \t]+   { /* Eat up whitespace. */ }
.            { return UNKNOWN; }

%%
