%{

#include "lexer.h"

%}

%option noyywrap
%option nounput
%option noinput

%%

"true"|"false"              { return BOOL; } // boolean
b'.{1,2}'                   { return CHAR; } // char literals
b\"([^\"]|"\\\"")+\"        { return STRING; } // string literals
"//"(.+)[\n]                { count++; }    // inline comments
"/*".+"*/"|"\""|'           { position++; } // comments and quotes
"."|"-"|"*"|"!"|"+"|"/"|"%"|"&"|"|"|"^"|"<<"|">>"|".."|"..."|"||"|"&&"|"=="|"!="|"<"|">"|"<="|">="|"+="|"-="|"*="|"/="|"%="|"&="|"|="|"="|"^="|"<<="|">>=" { return OPERATOR; }
"::"|"->"|"=>"|"#"|"#!"|"’"|"$"|"["|"]"|"("|")"|"{"|"}"|","|";" { return SYMBOL;}
"bool"|"u8"|"u16"|"u32"|"u64"|"i8"|"i16"|"i32"|"i64"|"f32"|"f64"|"usize"|"isize"|"char"|"str"   { return PRIMITIVE; }
"abstract"|"alignof"|"as"|"be"|"box"|"break"|"const"|"continue"|"crate"|"do"|"else"|"enum"|"extern"|"final"|"fn"|"for"|"if"|"impl"|"in"|"let"|"loop"|"macro"|"macro_rules"|"match"|"mod"|"move"|"mut"|"offsetof"|"override"|"priv"|"pub"|"pure"|"ref"|"return"|"sizeof"|"static"|"self"|"struct"|"super"|"trait"|"type"|"typeof"|"unsafe"|"unsized"|"use"|"virtual"|"where"|"while"|"yield" { return KEYWORD; }
[a-zA-Z_]([_a-zA-Z0-9]+)?   { return ID; } // Identifiers
[0-9]([0-9_]+)?             { return DECIMAL; }
[ \t\r]                     { position++; } // whitespace
[\n]                        { return NEWLINE; }
.                           { position++; return UNKNOWN; }

%%