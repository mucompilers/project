#ifndef LEXER_H_
#define LEXER_H_

#include <stdio.h>

enum {
    BOOL = 256,
    ID,
    KEYWORD,
    PRIMITIVE,
    OPERATOR,
    DECIMAL,
    SYMBOL,
    CHAR,
    STRING,
    NEWLINE,
    UNKNOWN,
};

int count, position;

char *yytext;

int yylex(void);

FILE *yyin;

#endif
