%{

#include "parser.h"

%}

%option noyywrap
%option nounput
%option noinput

%%

";"          { return ';'; }
"("          { return LPAREN; }
")"          { return RPAREN; }
"+"          { return PLUS; }
"-"          { return '-'; }
"*"          { return '*'; }
"\\"         { return '\\'; }
[0-9]+       { yylval.num = atoi(yytext); return NUM; }
[ \n\r\t]+   { /* Eat up whitespace. */ }
.            { return UNKNOWN; }

%%
