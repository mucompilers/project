/* Assignment 2   */
/* CS4430         */
/* Feb 18, 2015   */
/* Quinton Miller */
/* Jack Boening   */

%{

#include "lexer.h"

%}

%option noyywrap
%option nounput
%option noinput

%%

"/*"((\*+[^/*])|([^*]))*\**"*/" { }
"//".*[\n\t] { }
[\n\r\t ]+ { /*EAT UP WHITESPACE*/ }

"abstract" { return ABSTRACT; }
"alignof" { return ALIGNOF; }
"as" { return AS; }
"be" { return BE; }
"box" { return BOX; }
"break" { return BREAK; }
"const" { return CONST; }
"continue" { return CONTINUE; }
"crate" {return CRATE; }
"do" {return DO; }
"else" {return ELSE; }
"enum" {return ENUM; }
"extern" {return EXTERN; }
"final" {return FINAL;}
"fn" { return FN; }
"for" {return FOR; }
"if" { return IF; }
"impl" { return IMPL; }
"in" { return IN; }
"let" { return LET; }
"loop" { return LOOP; }
"macro" { return MACRO; }
"macro_rules" { return MACRO_RULES; }
"match" { return MATCH; }
"mod" { return MOD; }
"move" { return MOVE; }
"mut" { return MUT; }
"offsetof" { return OFFSETOF; }
"override" { return OVERRIDE; }
"priv" { return PRIV; }
"pub" { return PUB; }
"pure" { return PURE; }
"ref" { return REF; }
"return" { return RETURN; }
"sizeof" { return SIZEOF; }
"static" { return STATIC; }
"self" { return SELF; }
"struct" { return STRUCT; }
"super" { return SUPER; }
"trait" { return TRAIT; }
"type" { return TYPE; }
"typeof" { return TYPEOF; }
"unsafe" { return UNSAFE; }
"unsized" { return UNSIZED; }
"use" { return USE; }
"virtual" { return VIRTUAL; }
"where" { return WHERE; }
"while" { return WHILE; }
"yield" { return YIELD; }
 
 
"bool" { return BOOL;  }
"u8" { return UEIGHT; }
"u16" { return USIXTEEN; }
"u32" { return UTHREETWO; }
"u64" { return USIXFOUR; }
"i8" { return IEIGHT; }
"i16" { return ISIXTEEN; }
"i32" { return ITHREETWO; }
"i64" { return ISIXFOUR; }
"f32" { return FTHREETWO; }
"f64" { return FSIXFOUR; }
"usize" { return USIZE; }
"isize" { return ISIZE; }
"char" { return CHAR; }
"str" { return STR; }
 
 
"::" { return CLASSACCESS; }
"->" { return OBJECTACCESS; }
"=>" { return PIPE; }
"#" { return POUND; }
"#!" { return POUNDEXCLAMATION; }
"'" { return APOSTRAPHE; }
"$" { return DOLLAR; }
"[" { return LBRACKET; }
"]" { return RBRACKET; }
"(" { return LPAREN; }
")" { return RPAREN; }
"{" { return LSQUIGBRACKET; }
"}" { return RSQUIGBRACKET; }
"," { return COMMA; }
";" { return SEMICOLON; }
 
 
"-" { return SUB; }
"*" { return MUL; }
"!" { return NOT; }
"+" { return ADD; }
"/" { return DIV; }
"%" { return MODOP; }
"&" { return AND; }
"|" { return OR; }
"^" { return EXCLUSIVEOR; }
"<<" { return LEFTSHIFT; }
">>" { return RIGHTSHIFT; }
"." { return PERIOD; }
".." { return WILDCARD; }
"..." { return THREEDOTS; }
"||" { return LOGICALOR; }
"&&" { return LOGICALAND; }
"==" { return EQUALTO; }
"!=" { return NOTEQUAL; }
"<" { return LESSTHAN; }
">" { return GREATERTHAN; }
"<=" { return LESSTHANOREQUAL; }
">=" { return GREATERTHANOREQUAL; }
"+=" { return PLUSEQUAL; }
"-=" { return SUBEQUAL; }
"*=" { return TIMESEQUAL; }
"/=" { return DIVEQUAL; }
"%=" { return MODEQUAL; }
"&=" { return ANDEQUAL; }
"|=" { return OREQUAL; }
"^=" { return EXCLUSIVEOREQUAL; }
"<<=" { return LEFTSHIFTEQUAL; }
">>=" { return RIGHTSHIFTEQUAL; }
"=" { return EQU; }

  
b\'\\?.\' { return LITCHAR; }
b\"([^\"]|\\\")+\" { return LITSTRING; }
[0-9]([0-9]|_)* { return LITDEC; }
"true"|"false" { return LITBOOL; }

([a-zA-Z]|_)([a-zA-Z]|[0-9]|_)* { return ID; }

. { return UNKNOWN; }

%%