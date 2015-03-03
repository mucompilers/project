#include <stdio.h>
#include <ctype.h>

#define MAX 100

typedef struct stack {
    int data[MAX];
    int top;
} stack;

int priority(char);

void init(stack *);

int empty(stack *);

int full(stack *);

char pop(stack *);

void push(stack *, char);

char top(stack *);

