%{

#include "lexer.h"

%}

%option noyywrap
%option nounput
%option noinput

%%

[\/][\*]((\*([\x00-\x7E]{-}[\/]))|([\x00-\x7E]{-}[\*]))*[\*][\/] {}

"+="			{ return PLUS_ASSIGN; }
"-="			{ return SUB_ASSIGN; } 
"*="			{ return MUL_ASSIGN; }
[\/][=]			{ return DIV_ASSIGN; }  
[\%][=]			{ return REM_ASSIGN; }
"&="			{ return BITAND_ASSIGN; } 
"^="			{ return BITXOR_ASSIGN; }
"|="			{ return BITOR_ASSIGN; }
"<<="			{ return SHL_ASSIGN; }
">>="			{ return SHR_ASSIGN; }
"::"			{ return PATH; }
"..."			{ return TRIPLE_DOT; }
".."			{ return RANGE; }
"."				{ return DOT; }
"->"			{ return FUNCTION_ARROW; } 
"=>"			{ return MATCH_ARROW; } 
"#"				{ return DIRECTIVE; } 
"#!"			{ return DIRECTIVE_FEATURE; } 
"'"				{ return APOST; }  
"$"				{ return DOLLAR; } 
"["				{ return LSQUARE; } 
"]"				{ return RSQUARE; } 
"("				{ return LPAREN; } 
")"				{ return RPAREN; } 
"{"				{ return LCURLY; } 
"}"				{ return RCURLY; } 
","				{ return COMMA; }
":"				{ return COLON; }
";"				{ return SEMICOLON; }
"*"				{ return ASTERISK; } 
[\/] 			{ return DIV; }
[\%] 			{ return REM; }
"+"  			{ return ADD; }
"-" 			{ return NEGATIVE_SIGN; }
"<<"  			{ return SHL; }
">>"			{ return SHR; }
"&"				{ return BITAND; }
"^"				{ return BITXOR; }
"|"				{ return BITOR; }
"=="			{ return EQ; }
"!="			{ return NE; }
"<"				{ return LT; }
">"				{ return GT; }
"<="			{ return LE; } 
">="			{ return GE; }
"&&"			{ return BOOL_AND; }
"||"			{ return BOOL_OR; }
"="				{ return ASSIGNMENT; }

"!"				{ return LOGICAL_NEGATE; }
[_]*[0-9]+[_0-9]*  		{ return LITDEC; } 
"abstract"	 	{ return ABSTRACT; }
"alignof"	  	{ return ALIGNOF; }
"as"		  	{ return AS; }
"be"		  	{ return BE; }
"box"		  	{ return BOX; }
"break"		  	{ return BREAK; }
"case"			{return  CASE;}
"const"		  	{ return CONST; }
"continue"	  	{ return CONTINUE; }
"crate"		  	{ return CRATE; }
"do"		  	{ return DO; }
"else"		  	{ return ELSE; }
"enum"		  	{ return ENUM; }
"extern"	  	{ return EXTERN; }
"final"		  	{ return FINAL; }
"fn"		  	{ return FN; }
"for"		  	{ return FOR; }
"if"		  	{ return IF; }
"impl"		  	{ return IMPL; }
"in"		  	{ return IN; }
"let"		  	{ return LET; }
"loop"		  	{ return LOOP; }
"macro"		  	{ return MACRO; }
"macro_rules" 	{ return MACRO_RULES; }
"match"		  	{ return MATCH; }
"mod"		  	{ return MOD; }
"move"		  	{ return MOVE; }
"mut"		  	{ return MUT; }
"offsetof"	  	{ return OFFSETOF; }
"override"	  	{ return OVERRIDE; }
"priv"	 	  	{ return PRIV; }
"pub"		 	{ return PUB; }
"pure"		  	{ return PURE; }
"ref"		  	{ return REF; }
"return"	  	{ return RETURN; }
"sizeof"	 	{ return SIZEOF; }
"static"	  	{ return STATIC; }
"self"	 	 	{ return SELF; }
"struct"	  	{ return STRUCT; }
"super"	 	  	{ return SUPER; }
"trait"		  	{ return TRAIT; }
"type" 		  	{ return TYPE; }
"typeof"	  	{ return TYPEOF; }
"unsafe" 	  	{ return UNSAFE; }
"unsized"	  	{ return UNSIZED; }
"use"		  	{ return USE; }
"virtual" 	  	{ return VIRTUAL; }
"where" 	  	{ return WHERE; }
"while" 	  	{ return WHILE; }
"yield"		  	{ return YIELD; }
"bool" 				{ return BOOL; }
"u8" 				{ return U8; }
"u16"			 	{ return U16; }
"u32" 				{ return U32; }
"u64"				{ return U64; }
"i8"		 		{ return I8; }
"i16" 				{ return I16; }
"i32"				{ return I32; }
"i64" 				{ return I64; }
"f32"				{ return F32; }
"f64" 				{ return F64; }
"usize"				{ return USIZE; }
"isize" 			{ return ISIZE; }
"char" 				{ return CHAR; }	  
"str"				{ return STR;}

"true"				{return LITBOOL;}
"false"				{return LITBOOL;}

[b][\"]((([ -~]{-}[\\\"]))|((\\)(\\|\")))*[\"] {return LITSTR;}

[_a-zA-Z]+[_a-zA-Z0-9]*	  { return ID; }

[b][\'](([\x00-\xFF]{-}[\\\'])|((\\)(\\|\')))[\']				{ return LITCHAR; }


[\/][\/][\x00-\x7E]*          { }






[ \n\r\t]+   { /* Eat up whitespace. */ }
.            { return UNKNOWN; }
%%
