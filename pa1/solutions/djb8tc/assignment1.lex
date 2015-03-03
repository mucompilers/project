%option noyywrap
%s IN_COMMENT
%%	
<INITIAL>{
"/*"              BEGIN(IN_COMMENT);
	/*WHITESPACE*/
[ \t] {  }
	/*COMMENTS*/
\/\/.*\n { ++yylineno; }
	/*KEYWORD*/
abstract|alignof|as|be|become|box|break|const|continue|crate|do|else|enum|extern|final|fn|for|if|impl|in|let|loop|macro|macro_rules|match|mod|move|mut|offsetof|override|priv|pub|pure|ref|return|sizeof|static|self|struct|super|trait|type|typeof|unsafe|unsized|use|virtual|where|while|yield { printf("%s\n", yytext); }
	/*LITBOOL*/
true|false { printf("LITBOOL(%s)\n", yytext); }
	/*PRIMITIVE TYPES*/
bool|[ui]8|[ui]16|[uif]32|[uif]64|[ui]size|char|str { printf("%s\n", yytext); }
	/*ID*/
[a-zA-Z_][a-zA-Z_0-9]* { printf("ID(%s)\n", yytext); }
	/*LITCHAR*/
b\'(\\\\|\\\'|[^\\\'])\' { if (yytext[2] == '\\') { printf("LITCHAR(%c)\n", yytext[3]); } else { printf("LITCHAR(%c)\n", yytext[2]); } }
	/*LITSTR*/
b\"(\\\\|\\\"|[^\\\"\n])+\" { int i = 2; printf("LITSTR("); while (yytext[i] != '"') { if (yytext[i] == '\\') { printf("%c", yytext[++i]); } else { printf("%c", yytext[i]); } ++i; } printf(")\n"); }
	/*LITDEC*/
[0-9][0-9_]* { int i = 0; printf("LITDEC("); while (i < yyget_leng()) { while (yytext[i] == '_') { ++i; } printf("%c", yytext[i]); ++i; } printf(")\n"); }
	/*SYMBOL*/
::|->|#!?|\[|\]|\(|\)|\{|\}|,|;|=>|\'|\$ { printf("%s\n", yytext); }
	/*OPERATOR*/
[<>]?[<>]=|[=!+\-*/%&\|^]?=|-|\*|!|\+|\/|%|&&?|\|\|?|\^|<<?|>>?|\.\.?\.? { printf("%s\n", yytext); }
	/*ERRORS*/
. { printf("ERROR! Line %d: Invalid Token: %s\n", yylineno, yytext); }
	/*NEW LINE*/
\n { ++yylineno; }
}
<IN_COMMENT>{
"*/"      BEGIN(INITIAL);
[^*\n]+   
"*"       
\n        yylineno++;
}
%%
int main(void) {
	yylex();
}