%{

#include "lexer.h"

%}

%option noyywrap
%option nounput
%option noinput

%%

"("          { return SYMB; }
")"          { return SYMB; }
"}"          { return SYMB; }
"{"          { return SYMB; }
"["          { return SYMB; }
"]"          { return SYMB; }
"#"          { return SYMB; }
"#!"         { return SYMB; }
"->"         { return SYMB; }
"=>"         { return SYMB; }
"$"          { return SYMB; }
"::"         { return SYMB; }
","          { return SYMB; }
";"          { return SYMB; }
"!"          { return SYMB; }
"%"          { return SYMB; }
"&"          { return SYMB; }
"|"          { return SYMB; }
"^"          { return SYMB; }
"<<"         { return SYMB; }
">>"         { return SYMB; }
"||"         { return SYMB; }
"&&"         { return SYMB; }
"=="         { return SYMB; }
"!="         { return SYMB; }
"<"          { return SYMB; }
">"          { return SYMB; }
"<="         { return SYMB; }
">="         { return SYMB; }
"+="         { return SYMB; }
"-="         { return SYMB; }
"*="         { return SYMB; }
"/="         { return SYMB; }
"%="         { return SYMB; }
"&="         { return SYMB; }
"|="         { return SYMB; }
"^="         { return SYMB; }
"<<="        { return SYMB; }
">>="        { return SYMB; }
".."         { return SYMB; }
"..."        { return SYMB; }
"\'"          { return OP; }
"\""          { return OP; }

"+"          { return OP; }
"-"          { return OP; }
"*"          { return OP; }
"="          { return OP; }
"\\"          { return OP; }

"abstract"   { return KEYWORD; }
"alignof"   { return KEYWORD; }
"as"   { return KEYWORD; }
"be"   { return KEYWORD; }
"box"   { return KEYWORD; }
"break"   { return KEYWORD; }
"const"   { return KEYWORD; }
"continue"   { return KEYWORD; }
"crate"   { return KEYWORD; }
"do"   { return KEYWORD; }
"else"   { return KEYWORD; }
"enum"   { return KEYWORD; }
"extern"   { return KEYWORD; }
"final"   { return KEYWORD; }
"fn"   { return KEYWORD; }
"for"   { return KEYWORD; }
"if"   { return KEYWORD; }
"impl"   { return KEYWORD; }
"in"   { return KEYWORD; }
"let"   { return KEYWORD; }
"loKEYWORD"   { return KEYWORD; }
"macro"   { return KEYWORD; }
"macro_rules"   { return KEYWORD; }
"match"   { return KEYWORD; }
"mod"   { return KEYWORD; }
"move"   { return KEYWORD; }
"mut"   { return KEYWORD; }
"offsetof"   { return KEYWORD; }
"override"   { return KEYWORD; }
"priv"   { return KEYWORD; }
"pub"   { return KEYWORD; }
"pure"   { return KEYWORD; }
"ref"   { return KEYWORD; }
"return"   { return KEYWORD; }
"sizeof"   { return KEYWORD; }
"static"   { return KEYWORD; }
"self"   { return KEYWORD; }
"struct"   { return KEYWORD; }
"super"   { return KEYWORD; }
"trait"   { return KEYWORD; }
"type"   { return KEYWORD; }
"typeof"   { return KEYWORD; }
"unsafe"   { return KEYWORD; }
"unsized"   { return KEYWORD; }
"use"   { return KEYWORD; }
"virtual"   { return KEYWORD; }
"where"   { return KEYWORD; }
"while"   { return KEYWORD; }
"yield"   { return KEYWORD; }

[0-9]+       { return LITDEC; }
[a-z]+       { return ID; }



[ \n\r\t]+   { /* Eat up whitespace. */ }
.            { return UNKNOWN; }

%%
