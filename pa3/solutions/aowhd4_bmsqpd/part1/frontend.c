#include <stdio.h>
#include <glib.h>
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "env.h"

static GList* crate;

void parse_done(GList* items) {
      crate = items;
}

void yyerror(char const *s) {
      fprintf(stderr, "%s\n", s);
}

int main(int argc, char **argv) {
	  environment = env_new();
	  is_main = 0;
	  is_error = 0;
	  
      if (!yyparse()) {
      		if (!is_main) {
	  			printf("Error: main function not found.\n");
	  			return 0;
	  		}
	  		if (!is_error) {
	  			printf("Ok.\n");
	  		}
            //crate_print(crate);
            crate_destroy(crate);
            yylex_destroy();
      }
}
