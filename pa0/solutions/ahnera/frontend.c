#include "lexer.h"
#include "parse.c"
#include <stdio.h>
#include <stdlib.h>

static void token_print(int token) {
    switch (token) {
        case LPAREN:
            printf("LPAREN\n");
            break;
        case RPAREN:
            printf("RPAREN\n");
            break;
        case ADD:
            printf("ADD\n");
            break;
        case SUB:
            printf("SUB\n");
            break;
        case MUL:
            printf("MUL\n");
            break;
        case DIV:
            printf("DIV\n");
            break;
        case EXP:
            printf("EXP\n");
            break;
        case NUM:
            printf("NUM(%s) - %d\n", yytext, token);
            break;
        default:
            fprintf(stderr, "Error: unknown character: %s\n", yytext);
    }
}


int main(void) {
    //init(&s);
    while((token = yylex())){
        token = yylex();
        //parse();
    }

    exit(0);
}

