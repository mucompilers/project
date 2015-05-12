#ifndef LEXER_H_
#define LEXER_H_

enum {
      SYMB = 256,
      KEYWORD,
      OP,
      LITDEC,
      ID,
      LITCHAR,
      UNKNOWN,
};

char* yytext;
int yylex(void);

#endif
