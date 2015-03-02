%{

#include "lexer.h"

%}

%option noyywrap
%option nounput
%option noinput
%option yylineno
%s IN_COMMENT

%%

<INITIAL>{
"/*"					BEGIN(IN_COMMENT);
}
<IN_COMMENT>{
"*/"					BEGIN(INITIAL);
[^*\n]+					//eat comment in chunks
"*"						//eat lone star
\n						yylineno++;
}
"//"[^\r\n]* 			{ /* single line comment */}


"abstract"          	{ return ABSTRACT; }
"alignof"           	{ return ALIGNOF; }
"as"        	    	{ return AS; }
"be"         	    	{ return BE; }
"box"        	    	{ return BOX; }
"break"       	    	{ return BREAK; }
"const"					{ return CONST; }
"continue"          	{ return CONTINUE; }
"crate"           		{ return CRATE; }
"do"        	    	{ return DO; }
"else"         	    	{ return ELSE; }
"enum"        	    	{ return ENUM; }
"extern"       	    	{ return EXTERN; }
"final"					{ return FINAL; }
"fn"          			{ return FN; }
"for"           		{ return FOR; }
"if"        	    	{ return IF; }
"impl"         	    	{ return IMPL; }
"in"        	    	{ return IN; }
"let"       	   	    { return LET; }
"loop"					{ return LOOP; }
"macro"         	    { return MACRO; }
"macro_rules"           { return MACRO_RULES; }
"match"        	    	{ return MATCH; }
"mod"         	    	{ return MOD; }
"move"        	    	{ return MOVE; }
"mut"       	    	{ return MUT; }
"offsetof"				{ return OFFSETOF; }
"override"          	{ return OVERRIDE; }
"priv"          	    { return PRIV; }
"pub"        	    	{ return PUB; }
"pure"         	   	    { return PURE; }
"ref"        	    	{ return REF; }
"return"       	    	{ return RETURN; }
"sizeof"				{ return SIZEOF; }
"static"          		{ return STATIC; }
"self"           		{ return SELF; }
"struct"        	    { return STRUCT; }
"super"         	    { return SUPER; }
"trait"        	    	{ return TRAIT; }
"type"       	   	    { return TYPE; }
"typeof"				{ return TYPEOF; }
"unsafe"          		{ return UNSAFE; }
"unsized"         	    { return UNSIZED; }
"use"        	    	{ return USE; }
"virtual"         	    { return VIRTUAL; }
"where"        	    	{ return WHERE; }
"while"       	    	{ return WHILE; }
"yield"					{ return YIELD; }


"::"					{ return DBLCOL; }
"->"					{ return SNGLARROW; }
"=>"					{ return DBLARROW; }
"#"						{ return POUND; }
"#!"					{ return BASH; }
"$"						{ return DOLLAR; }
"["						{ return LBRACK; }
"]"						{ return RBRACK; }
"("						{ return LPAREN; }
")"						{ return RPAREN; }
"{"						{ return LBRACE; }
"}"						{ return RBRACE; }
","						{ return COMMA; }
";"						{ return SEMI; }
"-"						{ return DASH; }
"*"						{ return ASTER; }
"!"						{ return EXCLAM; }
"+"						{ return PLUS; }
"/"						{ return SLASH; }
"%"						{ return PERCENT; }
"&"						{ return AMP; }
"|"						{ return BAR; }
"^"						{ return CARAT; }
"<<"					{ return DBLLEFT; }
">>"					{ return DBLRIGHT; }
"||"					{ return DBLBAR; }
"&&"					{ return DBLAMP; }
"=="					{ return DBLEQ; }
"!="					{ return NOTEQ; }
"<"						{ return LTHAN; }
">"						{ return GTHAN; }
"<="					{ return LEQTHAN; }
">="					{ return GEQTHAN; }
"+="					{ return PLUSEQ; }
"-="					{ return MINEQ; }
"*="					{ return ASTEQ; }
"/="					{ return DASHEQ; }
"%="					{ return PERCEQ; }
"&="					{ return AMPEQ; }
"|="					{ return BAREQ; }
"^="					{ return CARATEQ; }
"<<="					{ return DBLLEFTEQ; }
">>="					{ return DBLRIGHTEQ; }
"..."					{ return ELIPSE; }
".."					{ return DBLPER; }
"="						{ return EQUAL; }
"."						{ return PERIOD; }


"bool"					{ return BOOL; }
"u8"					{ return U8; }
"u16"					{ return U16; }
"u32"					{ return U32; }
"u64"					{ return U64; }
"i8"					{ return I8; }
"i16"					{ return I16; }
"i32"					{ return I32; }
"i64"					{ return I64; }
"f32"					{ return F32; }
"f64"					{ return F64; }
"usize"					{ return USIZE; }
"isize"					{ return ISIZE; }
"char"					{ return CHAR; }
"str"					{ return STR; }


[0-9][0-9_]*			{ return LITDEC; }
(true)|(false)			{ return LITBOOL; }
b'([^'\\\n]|\\.)'		{ return LITCHAR; }
b\"([^\\\"]|\\.)*\"		{ return LITSTR; }
[a-zA-Z_][a-zA-Z0-9_]*	{ return ID; }


\n						{ /* Eat newline for yylineno */ }
[ \r\t]+   				{ /* Eat up whitespace. */ }
.           	    	{ return UNKNOWN; }

%%
