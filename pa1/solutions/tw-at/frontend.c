/* Created by Tyler Waddington and Andrew Tod
Wednesday, Feb, 18
Compilers Spring 2015
*/


#include "lexer.h"
#include <stdio.h>
#include <string.h>




int length;

static void token_print(int token) {
      switch (token) {
			case ID:
				  printf("ID(%s)\n", yytext);
				  break;

            case LITDEC:
				  printf("LITDEC(");
				  int i = 0;
				  while(yytext[i] != '\0'){//removes underscores
						if(yytext[i] != '_'){
				  			printf("%c", yytext[i]);
						}
						i++;
				  }
				  printf(")\n");
                  break;

            case LITSTR:
				  printf("LITSTR(");
				  i = 2;
				  length = strlen(yytext)-1;
				  while(i<length){//removes underscores
						if(yytext[i] == '\\'){
							i++;
						}
				  			printf("%c", yytext[i]);
						i++;
				  }
				  printf(")\n");
                  break;

            case LITCHAR:
				  printf("LITCHAR(");
				  i = 2;
				  if(yytext[i] == '\\'){
				  	printf("%c", yytext[i+1]);
				  }
				  else{
				  	printf("%c", yytext[i]);
				  }	
				  printf(")\n");
                  break;
            case LITBOOL:
				  printf("LITBOOL(");
				  i = 2;
				  if(strcmp(yytext, "true")==0){
				  	printf("true");
				  }
				  else{
				  	printf("false");
				  }	
				  printf(")\n");
                  break;

/*Ambiguous Lexemes*/
	  		case NEGATIVE_SIGN:
				  printf("-\n");
				  break;
			case ASTERISK:
				  printf("*\n");
                  break;  
			case AS:
                  printf("as\n");
                  break;

/*Unary Operator*/

			case LOGICAL_NEGATE:
				  printf("!\n");
				  break;

/*Binary Operators*/
 
	 		case DIV:
				  printf("/\n");
                  break; 
 			case REM:
				  printf("%%\n");
                  break;
	  		case ADD:
				  printf("+\n");
                  break;

	  		case SHL: 
				  printf("<<\n");
            	   break;
			case SHR: 
				  printf(">>\n");
                  break;
			case BITAND: 
				  printf("&\n");
                  break;
			case BITXOR: 
				  printf("^\n");
                  break;
			case BITOR:
				  printf("|\n");
                  break;
			case EQ: 
				  printf("==\n");
                  break;
			case NE: 
				  printf("!=\n");
                  break;
			case LT: 
				  printf("<\n");
                  break;
			case GT: 
				  printf(">\n");
                  break;
			case LE: 
				  printf("<=\n");
                  break;
			case GE: 
				  printf(">=\n");
                  break;
			case BOOL_AND: 
				  printf("&&\n");
                  break;
			case BOOL_OR: 
				  printf("||\n");
                  break;
			case ASSIGNMENT: 
				  printf("=\n");
                  break;
			case RANGE:
				  printf("..\n");
                  break;
/*Compound Binary Operators*/

			case PLUS_ASSIGN:
				  printf("+=\n");
                  break;
			case SUB_ASSIGN: 
				  printf("-=\n");
                  break;
			case MUL_ASSIGN:
				  printf("*=\n");
                  break;
			case DIV_ASSIGN:  
				  printf("/=\n");
                  break;
			case REM_ASSIGN:
				  printf("%%=\n");
                  break;
			case BITAND_ASSIGN: 
				  printf("&=\n");
                  break;
			case BITXOR_ASSIGN:
				  printf("^=\n");
                  break;
			case BITOR_ASSIGN:
				  printf("|=\n");
                  break;
			case SHL_ASSIGN:
				  printf("<<=\n");
                  break;
			case SHR_ASSIGN:
				  printf(">>=\n");
                  break;
/*Symbols*/
			case PATH:				  
				  printf("::\n");
                  break;
			case FUNCTION_ARROW:
				  printf("->\n");
                  break;
			case MATCH_ARROW: 
				  printf("=>\n");
                  break;
				case DIRECTIVE: 
				  printf("#\n");
                  break;
			case DIRECTIVE_FEATURE:
				  printf("#!\n");
                  break; 
			case APOST:  
				  printf("'\n");
                  break;
			case DOLLAR:
				  printf("$\n");
                  break; 
			case LSQUARE: 
				  printf("[\n");
                  break;
			case RSQUARE: 
				  printf("]\n");
                  break;
			case LPAREN:
				  printf("(\n");
                  break; 
			case RPAREN:
				  printf(")\n");
                  break; 
			case LCURLY:
				  printf("{\n");
                  break; 
			case RCURLY:
				  printf("}\n");
                  break;
			case TRIPLE_DOT:
				  printf("...\n");
                  break;
			case DOT:
				  printf(".\n");
                  break; 
			case COMMA:
				  printf(",\n");
                  break;
			case SEMICOLON:
				  printf(";\n");
                  break;
			case COLON:
				  printf(":\n");
                  break;
/*Keywords*/
			case ABSTRACT:
                  printf("abstract\n");
                  break;
			case ALIGNOF:
                  printf("alignof\n");
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
			case CASE:
                  printf("case\n");
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
                  printf("if\n");
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
/*end keywords*/

/*date types*/

	 		case BOOL:
                  printf("bool\n");
                  break;
			case U8:
                  printf("u8\n");
                  break;
			case U16:
                  printf("u16\n");
                  break;
			case U32:
                  printf("u32\n");
                  break;
			case U64:                  
				  printf("u64\n");
                  break;
	 		case I8:
                  printf("i8\n");
                  break;
			case I16:
                  printf("i16\n");
                  break;
			case I32:
                  printf("i32\n");
                  break;
			case I64:
                  printf("i64\n");
                  break;
			case F32:
                  printf("f32\n");
                  break;
			case F64:
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

            default:
                  fprintf(stderr, "Error: unknown character: %s\n", yytext);
      }
}

int main(int argc, char**argv) {
      int token;
	  
/* For future work
	  FILE *in;
	  in = fopen("test.rs", "r");
	  YY_BUFFER_STATE ./new_buf;
	  new_buf = yy_create_buffer(in,100000);
	  yy_switch_to_buffer(new_buf);
*/

      while ((token = yylex())) {
            token_print(token);
      }
	  

      return 0;
}
