/* Assignment 2   */
/* CS4430         */
/* Feb 18, 2015   */
/* Quinton Miller */
/* Jack Boening   */

#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"

char* yytext; 
int yylex(void); 
int calc_litdec(char* in);

static void token_print(int token){
	switch(token){
		case ABSTRACT:
			printf("abstract\n");
			break;
		case ALIGNOF:
			printf("alignof\n");
			break; 
		case AS: 
			printf("as\n");
			break; 
		case BE: 
			printf("be\n");
			break; 
		case BOX: 
			printf("box\n");
			break; 
		case BREAK: 
			printf("break\n");
			break;
		case CONST: 
			printf("const\n");
			break; 
		case CONTINUE: 
			printf("continue\n");
			break; 
		case CRATE: 
			printf("crate\n");
			break; 
		case DO:
			printf("do\n");
			break; 
		case ELSE: 
			printf("else\n");
			break; 
		case ENUM:
			printf("enum\n");
			break;
		case EXTERN:
			printf("extern\n");
			break; 
		case FALSE:
			printf("false\n");
			break; 
		case FINAL:
			printf("final\n");
			break; 
		case FN:
			printf("fn\n");
			break;
		case FOR:
			printf("for\n"); 
			break; 
		case IF:
			printf("IF\n");
			break; 
		case IMPL: 
			printf("impl\n");
			break; 
		case IN:
			printf("in\n");
			break; 
		case LET: 
			printf("let\n");
			break; 
		case LOOP: 
			printf("loop\n");
			break; 
		case MACRO: 
			printf("macro\n");
			break; 
		case MACRO_RULES:
			printf("macro_rules\n");
			break; 
		case MATCH:
			printf("match\n");
			break; 
		case MOD: 
			printf("mod\n");
			break; 
		case MOVE:
			printf("move\n");
			break; 
		case MUT:
			printf("mut\n");
			break; 
		case OFFSETOF:
			printf("offsetof\n");
			break; 
		case OVERRIDE: 
			printf("override\n");
			break; 
		case PRIV:
			printf("priv\n");
			break; 
		case PUB: 
			printf("pub\n");
			break; 
		case PURE: 
			printf("pure\n");
			break; 
		case REF:
			printf("ref\n");
			break; 
		case RETURN:
			printf("return\n");
			break; 
		case SIZEOF:
			printf("sizeof\n");
			break; 
		case STATIC:
			printf("static\n");
			break; 
		case SELF:
			printf("self\n");
			break; 
		case STRUCT:
			printf("struct\n");
			break; 
		case SUPER:
			printf("super\n");
			break; 
		case TRUE: 
			printf("true\n");
			break; 
		case TRAIT:
			printf("trait\n");
			break; 
		case TYPE: 
			printf("type\n");
			break; 
		case TYPEOF:
			printf("typeof\n");
			break; 
		case UNSAFE:
			printf("unsafe\n");
			break; 
		case UNSIZED:
			printf("unsized\n");
			break; 
		case USE:
			printf("use\n");
			break; 
		case VIRTUAL:
			printf("virtual\n");
			break; 
		case WHERE:
			printf("where\n");
			break; 		
		case WHILE:
			printf("while\n");
			break; 
		case YIELD:
			printf("yield\n");
			break; 
		case BOOL: 
			printf("bool\n");
			break; 
		case UEIGHT: 
			printf("u8\n");
			break; 
		case USIXTEEN: 
			printf("u16\n");
			break; 
		case UTHREETWO: 
			printf("u32\n");
			break; 
		case USIXFOUR: 
			printf("u64\n");
			break; 
		case IEIGHT: 
			printf("i8\n");
			break; 
		case ISIXTEEN: 
			printf("i16\n");
			break; 
		case ITHREETWO: 
			printf("i32\n");
			break; 
		case ISIXFOUR:
			printf("i64\n");
			break;  
		case FTHREETWO:
			printf("f32\n");
			break; 
		case FSIXFOUR:
			printf("f64\n");
			break; 
		case USIZE: 
			printf("usize\n");
			break; 
		case ISIZE: 
			printf("isize\n");
			break; 
		case CHAR: 
			printf("char\n");
			break; 
		case STR:
			printf("str\n");
			break;
		case CLASSACCESS:
			printf("::\n");
			break; 
		case OBJECTACCESS: 
			printf("->\n");
			break; 
 		case PIPE: 
			printf("=>\n");
			break; 
		case POUND: 
			printf("#\n");
			break; 
		case POUNDEXCLAMATION: 
			printf("#!\n");
			break;
		case APOSTRAPHE:
			printf("'\n");
			break; 
		case DOLLAR: 
			printf("$\n");
			break; 
		case LBRACKET:
			printf("[\n");
			break; 
		case RBRACKET:
			printf("]\n");
			break; 
		case LPAREN:
			printf("(\n");
			break; 
		case RPAREN:
			printf(")\n");
			break; 
		case LSQUIGBRACKET:
			printf("{\n");
			break; 
		case RSQUIGBRACKET: 
			printf("}\n");
			break; 
		case COMMA:
			printf(",\n");
			break; 
		case SEMICOLON:
			printf(";\n");
			break;
		case SUB: 
			printf("-\n");
			break; 
		case MUL: 
			printf("*\n");
			break;
		case NOT: 
			printf("!\n");
			break;
		case ADD: 
			printf("+\n");
			break;
		case DIV: 
			printf("/\n");
			break;
		case MODOP: 
			printf("%%\n");
			break;
		case AND: 
			printf("&\n");
			break;
		case OR: 
			printf("|\n");
			break;
		case EXCLUSIVEOR: 
			printf("^\n");
			break;
		case LEFTSHIFT: 
			printf("<<\n");
			break;
		case RIGHTSHIFT: 
			printf(">>\n");
			break;
		case WILDCARD: 
			printf("..\n");
			break;
		case THREEDOTS: 
			printf("...\n");
			break;
		case LOGICALOR: 
			printf("||\n");
			break;
		case LOGICALAND: 
			printf("&&\n");
			break;
		case EQUALTO: 
			printf("==\n");
			break;
		case NOTEQUAL: 
			printf("!=\n");
			break;
		case LESSTHAN: 
			printf("<\n");
			break;
		case GREATERTHAN: 
			printf(">\n");
			break;
		case LESSTHANOREQUAL: 
			printf("<=\n");
			break;
		case GREATERTHANOREQUAL: 
			printf(">=\n");
			break;
		case PLUSEQUAL: 
			printf("+=\n");
			break;
		case SUBEQUAL: 
			printf("-=\n");
			break;
		case TIMESEQUAL: 
			printf("*=\n");
			break;
		case DIVEQUAL: 
			printf("/=\n");
			break;
		case MODEQUAL: 
			printf("%%=\n");
			break;
		case ANDEQUAL: 
			printf("&=\n");
			break;
		case OREQUAL: 
			printf("|=\n");
			break;
		case EXCLUSIVEOREQUAL: 
			printf("^=\n");
			break;
		case LEFTSHIFTEQUAL: 
			printf("<<=\n");
			break;
		case RIGHTSHIFTEQUAL: 
			printf(">>=\n");
			break; 
    case LITCHAR:
      printf("LITCHAR(");
      if (yytext[2] == '\\'){
        printf("%c", yytext[3]);
      }
      else{
        printf("%c", yytext[2]);
      }
      printf(")\n");
      break;
    case LITSTRING:
      printf("LITSTR(");
      int i, j = 0;
      for (i = 2; ; i++){
        if (j != 1 && yytext[i] == '\"')
          break;
        if (yytext[i] == '\\' && j == 0){
          j = 1;
        }else{
          j = 0;
          printf("%c", yytext[i]); 
        }
      }
      printf(")\n");
      break;
    case LITDEC:
      printf("LITDEC(%d)\n", calc_litdec(yytext));
      break;
    case LITBOOL:
      printf("LITBOOL(%s)\n", yytext);
      break;
    case ID:
      printf("ID(%s)\n", yytext);
      break;
    case EQU:
      printf("=\n");
      break;
    case PERIOD:
      printf(".\n");
      break;
		default: 
			fprintf(stderr, "Error, unknown token %s %d\n", yytext, token); 
	}
}

int main(void){ 
  yyparse();
	return 0; 
}

int calc_litdec(char* in){

  int i = 0, j = 0;
  char line[150];
  for (i = 0; i < 150 && in[i] != '\0'; i++){
    
    if (in[i] != '_'){
    
      line[j] = in[i];
      j++;
      
    }
    
  }
  
  return atoi(line);
}
