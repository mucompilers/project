#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAX 10
#define EMPTY -1

struct stack {
    char data[MAX];
    int top;
};

int isempty(struct stack *s) {
    return (s->top == EMPTY) ? 1 : 0;
}

void emptystack(struct stack *s) {
    s->top = EMPTY;
}

void push(struct stack *s, int item) {
    if (s->top == (MAX - 1)) {
        printf("\nSTACK FULL");
    }
    else {
        ++s->top;
        s->data[s->top] = item;
    }
}

char pop(struct stack *s) {
    char ret = (char) EMPTY;
    if (!isempty(s)) {
        ret = s->data[s->top];
        --s->top;
    }
    return ret;
}