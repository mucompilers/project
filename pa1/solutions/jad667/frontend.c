#include "lexer.h"
#include <stdio.h>
#include <string.h>

static void token_print(int token) {
      switch (token) {
            case KEYWORD:
                  printf("%s\n", yytext);
                  break;
            case SYMB:
                  printf("%s\n", yytext);
                  break;
            case OP:
                  printf("%s\n", yytext);
                  break;
            case LITCHAR:
//                  char *ch;
//                  ch = strtok(yytext, "\'");
//                  while (ch != NULL) {
//                      if(ch != 'b'){
//                          printf("%s\n", ch);
//                      }
//                      ch = strtok(NULL, "\'");
//                  }
                  printf("LITCHAR(%s)\n", yytext);
                  break;
            case LITDEC:
                  printf("LITDEC(%s)\n", yytext);
                  break;
            case ID:
                  printf("ID(%s)\n", yytext);
                  break;
            default:
                  fprintf(stderr, "Error: unknown character: %s\n", yytext);
      }
}

int main(void) {
      int token;

      while ((token = yylex())) {
            token_print(token);
      }

      return 0;
}
