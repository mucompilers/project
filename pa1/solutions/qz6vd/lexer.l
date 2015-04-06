%{

#include "lexer.h"

%}

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

symbol	"::"|"->"|"#"|"["|"]"|"("|")"|"{"|"}"|","|";"|"#!"|"'"|"=>"|"$"
operator	"-"|"*"|"!"|"+"|"/"|"%"|"&"|"|"|"^"|"<<"|">>"|"||"|"&&"|"=="|"!="|"<"|">"|"<="|">="|"as"|"="|"+="|"-="|"*="|"/="|"%="|"&="|"|="|"^="|"<<="|">>="|"..."|".."	
primitive	"()"|"bool"|"u8"|"u16"|"u32"|"u64"|"i8"|"i16"|"i32"|"i64"|"binary32"|"binary64"|"usize"|"isize"|"char"|"str"|"is"|"us"

%%

{symbol}		{ return SYMBOL;}
{operator}		{ return OPERATOR;}
{primitive}		{ return PRIMITIVE;}	



"macro_rules"	{ return KEYWORD;}
"offsetof"	{ return KEYWORD;}
"abstract"   { return KEYWORD;}
"alignof"    { return KEYWORD;}
"be" 	     { return KEYWORD;}
"box"	     { return KEYWORD;}
"break"      { return KEYWORD;}
"const"      { return KEYWORD;}
"continue"   { return KEYWORD;}
"crate"      { return KEYWORD;}
"do"   	     { return KEYWORD;}
"else"       { return KEYWORD;}
"enum" 	     { return KEYWORD;}
"extern"     { return KEYWORD;}
"final"      { return KEYWORD;}
"fn" 	     { return KEYWORD;}
"for"	     { return KEYWORD;}
"if"	     { return KEYWORD;}
"impl"	     { return KEYWORD;}
"in"	     { return KEYWORD;}
"let" 	     { return KEYWORD;}
"loop" 	     { return KEYWORD;}
"macro"      { return KEYWORD;}
"match"      { return KEYWORD;}
"mod" 	     { return KEYWORD;}
"move"	     { return KEYWORD;}
"mut" 	     { return KEYWORD;}
"offsetpf"   { return KEYWORD;}
"override"   { return KEYWORD;}
"priv"	     { return KEYWORD;}
"pub"	     { return KEYWORD;}
"pure"	     { return KEYWORD;}
"ref"	     { return KEYWORD;}
"return"     { return KEYWORD;}
"sizeof"     { return KEYWORD;}
"static"     { return KEYWORD;}
"self"	     { return KEYWORD;}
"struct"     { return KEYWORD;}
"super"	     { return KEYWORD;}
"trait"	     { return KEYWORD;}
"type"	     { return KEYWORD;}
"typeof"     { return KEYWORD;}
"unsafe"     { return KEYWORD;}
"unsized"    { return KEYWORD;}
"use"	     { return KEYWORD;}
"virtual"    { return KEYWORD;}
"where"	     { return KEYWORD;}
"while"	     { return KEYWORD;}
"yield"	     { return KEYWORD;}



"true"                         { return LITBOOL;}
"false"                        { return LITBOOL;}

{ident}				  { return ID;}
b'{character_byte}'                   { return LITCHAR;}
b\"{character_string}+\"                 { return LITSTR;}
{digit}({digit}|_)*                 { return LITDEC;}


[ \n\r\t]+      {
		   if(yytext[0]=='\n')
		       accumulator++;
		}
\/\*(\s|.)*?\*\/	{ }
\/\/[^\n]*	{ }


\.		{ return SYMBOL;}
. 		{
    		    printf("[ERROR]:Unknown character %s, at line %d\n ",yytext,accumulator);
    		return UNKNOWN;}


%%
