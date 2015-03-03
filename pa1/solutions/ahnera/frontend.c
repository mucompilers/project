#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <string.h>

static void token_print(int token);

void removeChar(char *str, char c);

char *remEscapeCharsAndTrim(char *str);

int main(int argc, char **argv) {
    int token;
    count = 1;

    // make sure there is a file to be read in
    if (argc != 2) {
        printf("ERROR: ./pa1 <input-file>\n");
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

    // lex the file
    while ((token = yylex())) {
        token_print(token);
    }

    return 0;
}

static void token_print(int token) {
    switch (token) {
        case ID:
            position += strlen(yytext);
            printf("ID(%s)\n", yytext);
            break;
        case BOOL:
            position += strlen(yytext);
            printf("BOOL(%s)\n", yytext);
            break;
        case DECIMAL:
            position += strlen(yytext);
            removeChar(yytext, '_');
            printf("LITDEC(%s)\n", yytext);
            break;
        case STRING:
            position += strlen(yytext);
            // remove b"
            yytext += 2;
            yytext = remEscapeCharsAndTrim(yytext);
            printf("LITSTRING(%s)\n", yytext);
            break;
        case CHAR:
            // adds to the position for errors
            position += strlen(yytext);

            // remove b'
            yytext += 2;
            yytext = remEscapeCharsAndTrim(yytext);
            printf("LITCHAR(%s)\n", yytext);
            break;
        case NEWLINE:
            // reset the position of the line
            position = 0;

            // add to the line count for errors
            count++;
            break;
        case OPERATOR:
        case KEYWORD:
        case SYMBOL:
        case PRIMITIVE:
            position += strlen(yytext);
            printf("%s\n", yytext);
            break;
        default:
            fprintf(stderr, "Error at line %d: unknown character: %s at position %d\n", count, yytext, position);
    }
}

char *remEscapeCharsAndTrim(char *str) {
    int i, j = 0, size = strlen(str);
    char *escape = malloc(sizeof(char) * size);

    // loop through the string
    for (i = 0; i < size; i++) {

        // skip over the escape char
        if (str[i] == '\\')
            i++;

        // remove ending quotes
        else if (i == (size - 1) && (str[i] == '\'' || str[i] == '\"'))
            break;

        // add letter to new string
        escape[j] = str[i];
        j++;
    }

    escape[j] = '\0';

    return escape;

}

void removeChar(char *str, char c) {
    char *pr = str, *pw = str;
    while (*pr) {
        *pw = *pr++;
        pw += (*pw != c);
    }
    *pw = '\0';
}