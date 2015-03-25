#include <stdio.h>
#include <stdlib.h>
#include "parser.h"

FILE *yyin;

int main(int argc, char **argv) {
      // make sure there is a file to be read in
      if (argc != 2) {
            printf("ERROR: ./pa2 <input-file>\n");
            return -1;
      }

      FILE *file = fopen(argv[1], "r");

      // make sure the file is valid
      if (!file) {
            printf("Couldn't open %s", argv[1]);
            return -1;
      }

      // set lex to read from it instead of defaulting to STDIN
      yyin = file;
      yyparse();
      printf("Successfully parsed. \n");
      return 0;
}
