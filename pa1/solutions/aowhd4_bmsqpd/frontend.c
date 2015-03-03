#include "lexer.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void token_print(int token) {
      switch (token) {
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
        	case DBLCOL:
        		printf("::\n");
        		break;
        	case SNGLARROW:
        		printf("->\n");
        		break;
        	case DBLARROW:
        		printf("=>\n");
        		break;
        	case POUND:
        		printf("#\n");
        		break;
        	case BASH:
        		printf("#!\n");
        		break;
        	case DOLLAR:
        		printf("$\n");
        		break;
        	case LBRACK:
        		printf("[\n");
        		break;
        	case RBRACK:
        		printf("]\n");
        		break;
        	case LPAREN:
        		printf("(\n");
        		break;
        	case RPAREN:
        		printf(")\n");
        		break;
        	case LBRACE:
        		printf("{\n");
        		break;
        	case RBRACE:
        		printf("}\n");
        		break;
        	case COMMA:
        		printf(",\n");
        		break;
        	case SEMI:
        		printf(";\n");
        		break;
        	case DASH:
        		printf("-\n");
        		break;
        	case ASTER:
        		printf("*\n");
        		break;
        	case EXCLAM:
        		printf("!\n");
        		break;
        	case PLUS:
        		printf("+\n");
        		break;
        	case SLASH:
        		printf("/\n");
        		break;
        	case PERCENT:
        		printf("%%\n");
        		break;
        	case AMP:
        		printf("&\n");
        		break;
        	case BAR:
        		printf("|\n");
        		break;
        	case CARAT:
        		printf("^\n");
        		break;
        	case DBLLEFT:
        		printf("<<\n");
        		break;
        	case DBLRIGHT:
        		printf(">>\n");
        		break;
        	case DBLBAR:
        		printf("||\n");
        		break;
        	case DBLAMP:
        		printf("&&\n");
        		break;
        	case DBLEQ:
        		printf("==\n");
        		break;
        	case NOTEQ:
        		printf("!=\n");
        		break;
        	case LTHAN:
        		printf("<\n");
        		break;
        	case GTHAN:
        		printf(">\n");
        		break;
        	case LEQTHAN:
        		printf("<=\n");
        		break;
        	case GEQTHAN:
        		printf(">=\n");
        		break;
        	case PLUSEQ:
        		printf("+=\n");
        		break;
        	case MINEQ:
        		printf("-=\n");
        		break;
        	case ASTEQ:
        		printf("*=\n");
        		break;
        	case DASHEQ:
        		printf("/=\n");
        		break;
        	case PERCEQ:
        		printf("%%=\n");
        		break;
        	case AMPEQ:
        		printf("&=\n");
        		break;
        	case BAREQ:
        		printf("|=\n");
        		break;
        	case CARATEQ:
        		printf("^=\n");
        		break;
        	case DBLLEFTEQ:
        		printf("<<=\n");
        		break;
        	case DBLRIGHTEQ:
        		printf(">>=\n");
        		break;
        	case ELIPSE:
        		printf("...\n");
        		break;
        	case DBLPER:
        		printf("..\n");
        		break;
        	case EQUAL:
        		printf("=\n");
        		break;
        	case PERIOD:
        		printf(".\n");
        		break;
        	case ID:
        		printf("ID(%s)\n", yytext);
        		break;
        	case LITDEC: {
        		int i, j = 0;
        		char removeUnderscore[1024];
        		for (i = 0; i < strlen(yytext); i++){
    				if (yytext[i] != '_')
        				removeUnderscore[j++] = yytext[i];
				}
				removeUnderscore[j] = '\0';
				printf("LITDEC(%s)\n", removeUnderscore);
        		break;
        	}
        	break;
        	case LITBOOL:
        		printf("LITBOOL(%s)\n", yytext);
        		break;
        	case LITSTR: {
        		int i, j = 0;
        		char grabString[1024];
        		for (i = 2; i < strlen(yytext) - 1; i++){
        			if (yytext[i] == '\\' && yytext[i+1] == '\\') {
        				grabString[j++] = yytext[++i];
        			} else if (yytext[i] == '\\' && yytext[i+1] == '"') {
        				grabString[j++] = yytext[++i];
        			} else {
        				grabString[j++] = yytext[i];
        			}
        		}
        		grabString[j] = '\0';
        		printf("LITSTR(%s)\n", grabString);
        		break;
        	}
        	break;
        	case LITCHAR: {
        		int i, j = 0;
        		char grabChar[1024];
        		for (i = 2; i < strlen(yytext) - 1; i++){
        			if (yytext[i] == '\\' && yytext[i+1] == '\\') {
        				grabChar[j++] = yytext[++i];
        			} else if (yytext[i] == '\\' && yytext[i+1] == '\'') {
        				grabChar[j++] = yytext[++i];
        			} else {
        				grabChar[j++] = yytext[i];
        			}
        		}
        		grabChar[j] = '\0';
        		printf("LITCHAR(%s)\n", grabChar);
        		break;
        	}
        	break;
            default:
                fprintf(stderr, "Error: unknown character: %s on line %d\n", yytext, yylineno);
      }
}

int main(void) {
      int token;

      while ((token = yylex())) {
            token_print(token);
      }

      return 0;
}