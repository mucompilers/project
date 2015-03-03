#include "parse.h"

stack s;
char x;
int token;

void parse(void) {
    if (isalnum(token))
        printf("%c", token);
    else if (token == LPAREN)
        push(&s, '(');
    else {
        if (token == RPAREN)
            while ((x = pop(&s)) != LPAREN)
                printf("%c", x);
        else {
            while (priority(token) <= priority(top(&s)) && !empty(&s)) {
                x = pop(&s);
                printf("%c", x);
            }
            push(&s, *yytext);
        }
        /*while (!empty(&s)) {
            x = pop(&s);
            printf("%c", x);
        }*/
    }
}

//---------------------------------------------
int priority(char x) {
    if (x == LPAREN)
        return (0);
    if (x == ADD || x == SUB)
        return (1);
    if (x == MUL || x == DIV)
        return (2);
    return (3);
}

//---------------------------------------------
void init(stack *s) {
    s->top = -1;
}

//---------------------------------------------
int empty(stack *s) {
    if (s->top == -1)
        return (1);
    else
        return (0);
}

//---------------------------------------------
int full(stack *s) {
    if (s->top == MAX - 1)
        return (1);
    else
        return (0);
}

//---------------------------------------------
void push(stack *s, char x) {
    s->top = s->top + 1;
    s->data[s->top] = x;
}

//---------------------------------------------
char pop(stack *s) {
    int x;
    x = s->data[s->top];
    s->top = s->top - 1;
    return (x);
}

//---------------------------------------------
char top(stack *s) {
    return (s->data[s->top]);
}