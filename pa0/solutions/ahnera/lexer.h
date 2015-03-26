#ifndef LEXER_H_
#define LEXER_H_

enum {
    LPAREN = 256,
    RPAREN,
    MUL,
    ADD,
    SUB,
    DIV,
    EXP,
    NUM,
    UNKNOWN,
};

char *yytext;

int yylex(void);

#endif
