%{
char* fixString(const char* str);
char fixChar(const char* str);
%}

%option noyywrap

WHITESPACE		[ \n\t\r]
ALPHA			[a-zA-Z]
DIGIT			[0-9]
ID				[_a-zA-Z][a-zA-Z0-9_]*
LITCHAR			b'([^']*?)'
LITSTR			b\"([^\"]*?)\"
LITDEC			{DIGIT}*_*{DIGIT}*
LITBOOL			true|false
%%
abstract		printf("ABSTRACT\n");
alignof			printf("ALIGNOF\n");
as				printf("AS\n");
be				printf("BE\n");
box				printf("BOX\n");
break			printf("BREAK\n");
const			printf("CONST\n");
continue		printf("CONTINUE\n");
crate			printf("CRATE\n");
do				printf("DO\n");
else			printf("ELSE\n");
enum			printf("ENUM\n");
extern			printf("EXTERN\n");
final			printf("FINAL\n");
fn				printf("FN\n");
for				printf("FOR\n");
if				printf("IF\n");
impl			printf("IMPL\n");
in				printf("IN\n");
let				printf("LET\n");
loop			printf("LOOP\n");
macro			printf("MACRO\n");
macro_rules		printf("MACRO_RULES\n");
match			printf("MATCH\n");
mod				printf("MOD\n");
move			printf("MOVE\n");
mut				printf("MUT\n");
offsetof		printf("OFFSETOF\n");
override		printf("OVERRIDE\n");
priv			printf("PRIV\n");
pub				printf("PUB\n");
pure			printf("PURE\n");
ref				printf("REF\n");
return			printf("RETURN\n");
sizeof			printf("SIZEOF\n");
static			printf("STATIC\n");
self			printf("SELF\n");
struct			printf("STRUCT\n");
super			printf("SUPER\n");
trait			printf("TRAIT\n");
type			printf("TYPE\n");
typeof			printf("TYPEOF\n");
unsafe			printf("UNSAFE\n");
unsized			printf("UNSIZED\n");
use				printf("USE\n");
virtual			printf("VIRTUAL\n");
where			printf("WHERE\n");
while			printf("WHILE\n");
yield			printf("YIELD\n");

bool			printf("bool\n");
u8				printf("u8\n");
u16				printf("u16\n");
u32				printf("u32\n");
u64				printf("u64\n");
i8				printf("i8\n");
i16				printf("i16\n");
i32				printf("i32\n");
i64				printf("i64\n");
f32				printf("f32\n");
f64				printf("f64\n");
usize			printf("usize\n");
isize			printf("isize\n");
char			printf("char\n");
str				printf("str\n");

{LITCHAR}		printf("LITCHAR(%c)\n", fixChar(yytext));
{LITSTR}		printf("LITSTR(%s)\n", fixString(yytext));
{LITBOOL}		printf("LITBOOL(%s)\n", yytext);
{ID}			printf("ID(%s)\n", yytext);
{LITDEC}		printf("LITDEC(%s)\n", yytext);
{WHITESPACE}	;

[:][:]			printf("::\n");
[-][>]			printf("->\n");
[=][>]			printf("=>\n");
[#]				printf("#\n");
[#][!]			printf("#!\n");
[$]				printf("$\n");
[+]				printf("+\n");
[+][=]			printf("+=\n");
[-]				printf("-\n");
[-][=]			printf("-=\n");
[*]				printf("*\n");
[*][=]			printf("*=\n");
[/]				printf("/\n");
[/][=]			printf("/=\n");
[=]				printf("=\n");
[=][=]			printf("==\n");
[(]				printf("(\n");
[)]				printf(")\n");
"["				printf("[\n");
"]"				printf("]\n");
[{]				printf("{\n");
[}]				printf("}\n");
[,]				printf(",\n");
[;]				printf(";\n");
[!]				printf("!\n");
[%]				printf("%\n");
[&]				printf("&\n");
"|"				printf("|\n");
"^"				printf("^\n");
"<<"			printf("<<\n");
">>"				printf(">>\n");
"<<=="			printf("<<==\n");
">>=="			printf(">>==\n");
"||"			printf("||\n");
"&&"			printf("&&\n");
"!="			printf("!=\n");
"<"				printf("<\n");
">"				printf(">\n");
"<="			printf("<=\n");
">="			printf(">=\n");
"%="			printf("%=\n");
"&="			printf("&=\n");
"|="			printf("|=\n");
"^="			printf("^=\n");
"."				printf(".\n");
".."			printf("..\n");
"..."			printf("...\n");

"/*".+|"\n"+"*/" ;		// Eat multiline comments
"//".+			;		// Eat single line comments
.				printf("UNKNOWN\n");
%%

int main(int argc, char *argv[])
{
	if (argc == 2)
		yyin = fopen(argv[1], "r");
	else if (argc == 1)
		yyin = stdin;
	else
	{
		printf("USAGE: %s [filename]\n", argv[0]);
		return 1;
	}

	int token;
	while(token = yylex())
		printf("%d\n", token);
	
	return 0;
}

char* fixString(const char* str)
{
	char* newStr;

	newStr = strtok(str, "\"");
	newStr = strtok(NULL, "\"");
//	printf("newStr -> %s\n", newStr);
	return newStr;
}

char fixChar(const char* str)
{
	if(str[2] != '\\')
		return str[2];
	return str[3];
}
