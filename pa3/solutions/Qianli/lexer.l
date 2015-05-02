%{
#include <glib.h>
#include "parser.h"

%}
%option yylineno
%option noyywrap
%option nounput
%option noinput

digit	[0-9]
letter	[a-zA-Z]

eol	{\n}	

XID_start	{[a-zA-Z_]}
XID_continue	{[a-zA-Z0-9_]*}
ident		[a-zA-Z_][a-zA-Z0-9_]*

character_byte	[^\\^']|(\\\\)|(\\')
character_string	[^\\^\"]|(\\\\)|(\\\")


comment		{block_comment|line_comment}
block_comment	{/\*{block_comment_body}\*/}
block_comment_body	{[block_comment|.]*}
line_comment	{//{^eol}*}

whitespace_char	[ \t\n\r]
whitespace	(whitespace_char|comment)+


%%
"_"	{ return UNDERSCORE;}
"-"	{ return MINUS;}
"*"	{ return MULTIPLE;}
"!"	{ return EXCLAMATORY;}
"+"	{ return PLUS;}
"/"	{ return DIVISION;}
"%"	{ return PERCENT;}
"&"	{ return AND_BIT;}
"|"	{ return OR_BIT;}
"^"	{ return XOR;}
"<<"	{ return LSHIFT;}
">>"	{ return RSHIFT;}
"||"	{ return OR;}
"&&"	{ return AND;}
"=="	{ return DOUBLE_EQUAL;}
"!="	{ return NOTEQUAL;}
"<"	{ return LESS;}
">" 	{ return MORE;}
"<="	{ return LESSOREQUAL;}
">="	{ return MOREOREQUAL;}
"as"	{ return AS;}
"="	{ return EQUAL;}
"+="	{ return PLUSEQUAL;}
"-="	{ return MINUSEQUAL;}
"*="	{ return MULEQUAL;}
"/="	{ return DIVEQUAL;}
"%="	{ return REMEQUAL;}
"&="	{ return ANDEQUAL;}
"|="	{ return OREQUAL;}
"^="	{ return XOREQUAL;}
"<<="	{ return LSHIFTEQUAL;}
">>="	{ return RSHIFTEQUAL;}
"..."	{ return TRIPOINT;}
".."	{ return DOUPOINT;}
"\."	{ return DOT;}


"()"	{ return EMPTY;}
"bool"	{ return BOOL;}
"u8"	{ return U8;}
"u16"	{ return U16;}
"u32"	{ return U32;}
"u64"	{ return U64;}
"i8"	{ return I8;}
"i16"	{ return I16;}
"i32"	{ return I32;}
"i64"	{ return I64;}
"binary32" 	{ return BINARY32;}
"binary64"	{ return BINARY64;}
"usize"		{ return USIZE;}
"isize"		{ return ISIZE;}
"char"		{ return CHAR;}
"str"		{ return STR;}
"is"		{ return IS;}
"us"		{ return US;}


"::"	{ return NAMESPACE;}
"->"	{ return POINTER;}
"#"	{ return SHARP;}
"["	{ return LBRACKET;}
"]"	{ return RBRACKET;}
"("	{ return LPARENT;}
")"	{ return RPARENT;}
"{"	{ return LBRACE;}
"}"	{ return RBRACE;}
","	{ return COMMA;}
";"	{ return SEMICOLON;}
"#!"	{ return ATTRIBUTE;}
"'"	{ return SINGLEQUOTA;}
"=>"	{ return EQUALTO;}
"$"	{ return DOLLAR;}
":"	{ return COLON;}




"new"	{return NEW;}
"macro_rules"	{ return MACRO_RULES;}
"offsetof"	{ return OFFSETOF;}
"abstract"   { return ABSTRACT;}
"alignof"    { return ALIGNOF;}
"be" 	     { return BE;}
"Box"	     { return BOX;}
"break"      { return BREAK;}
"const"      { return CONST;}
"continue"   { return CONTINUE;}
"crate"      { return CRATE;}
"do"   	     { return DO;}
"else"       { return ELSE;}
"enum" 	     { return ENUM;}
"extern"     { return EXTERN;}
"final"      { return FINAL;}
"fn" 	     { return FN;}
"for"	     { return FOR;}
"if"	     { return IF;}
"impl"	     { return IMPL;}
"in"	     { return IN;}
"let" 	     { return LET;}
"loop" 	     { return LOOP;}
"macro"      { return MACRO;}
"match"      { return MATCH;}
"mod" 	     { return MOD;}
"move"	     { return MOVE;}
"mut" 	     { return MUT;}
"offsetpf"   { return OFFSETPF;}
"override"   { return OVERRIDE;}
"priv"	     { return PRIV;}
"pub"	     { return PUB;}
"pure"	     { return PURE;}
"ref"	     { return REF;}
"return"     { return RETURN;}
"symbol"     { return SYMBOL;}
"sizeof"     { return SIZEOF;}
"static"     { return STATIC;}
"self"	     { return SELF;}
"struct"     { return STRUCT;}
"super"	     { return SUPER;}
"trait"	     { return TRAIT;}
"type"	     { return TYPE;}
"typeof"     { return TYPEOF;}
"unsafe"     { return UNSAFE;}
"unsized"    { return UNSIZED;}
"use"	     { return USE;}
"virtual"    { return VIRTUAL;}
"where"	     { return WHERE;}
"while"	     { return WHILE;}
"yield"	     { return YIELD;}



"true"                          { 
    				yylval.boolean = 1;
    				return BOOLEAN;}
"false"                         { 
    				yylval.boolean = 0;
    				return BOOLEAN;}

{ident}				{ 
    				yylval.id=malloc(strlen(yytext)*sizeof(char));
				strcpy(yylval.id, yytext);
				return ID;}
b'{character_byte}'            	{ 
    				yylval.charactor = yytext[2];
    				return LITCHAR;}
b\"{character_string}+\"        { 
				yylval.string = &(yytext[2]);
				return LITSTR;}
{digit}({digit}|_)*             { 
    				yylval.number = atoi(yytext);
    				return LITDEC;}


[ \r\t\n]+      {	}
\/\*(\s|.)*?\*\/	{ }
\/\/[^\n]*	{ }


. 		{
    		return UNKNOWN;}


%%
