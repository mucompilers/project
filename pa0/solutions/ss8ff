#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct node { //define node structure
    int num;
    int op;
    struct node* left;
    struct node* right;
    struct node* previous;
};

int parse_num(struct node * n) { //parse numbers
    int c = getchar();
    while (c == 32) //ignore space 
        c = getchar();
    if (c == 10) // end of string
        return 1;
    if (c > 57 || (c > 32 && c < 48)) { //check for error input
       return 0;
        printf("Error input in the number field(s)!\n");
        exit(EXIT_SUCCESS);
    }
    int next, total = 0;
    struct node * new = malloc(sizeof(struct node));
    new->previous = n;
    while (isdigit(c)) //if c is a number
    {                  //scan till the last digit of a number and put them together
        total *= 10;
        next = c - '0';
        total += next;
        c = getchar();
    }
    ungetc(c, stdin); //return the character back to the string if it's not a number
    new->num = total; //assign the value to the num field ofnew node 
    if (n->left == NULL)
        n->left = new;
    else {
        n->right = new;
    }
    return 0;
}

int parse_op(struct node * n) { //parse operation signs
    int c = getchar();
    if (c == 10) // end of string
        return 1;
    while (c == 32) //ignore space 
        c = getchar();
    //check for error input
    //ascii number 10 is linefeed, which is generated when pressing enter key. Therefore it is excluded.
    if (c != 10 && c != '+' && c != '-' && c != '*' && c != '/') {
        printf("Error input in the operation field(s)!\n");
        exit(EXIT_SUCCESS);
       return 1;
    }
    switch (c) { //assign a number to each sign
        case '+':
        n->op = 1;
        break;
        case '-':
        n->op = 2;
        break;
        case '*':
        n->op = 3;
        break;
        case '/':
        n->op = 4;
        break;
        default:
        ungetc(c, stdin);
        break;
        }
        return 0; //return the sign that corresponds to each different sign
}

int parse_lparen(struct node * n) { //parse the left parenthesis
    int c  = getchar();
    if (c == 10) // end of string
        return 1;
    while (c == 32) //ignore space 
        c = getchar();
    while (c == '(') {
        struct node * new = malloc(sizeof(struct node));
        new->previous = n;
        if (n->left == NULL) 
            n->left = new;
        else 
            n->right = new;
        n = new;
        c = getchar();
    }
        ungetc(c, stdin);
        return 0;
}

int parse_rparen(struct node * n) { //parse the right parenthesis
    int c = getchar();
    if (c == 10) // end of string
        return 1;
    while (c == 32) //ignore space 
        c = getchar();
    while (c == ')') {
        n = n->previous;
        c = getchar();
    }
        ungetc(c, stdin);
        return 0;
}

int parse(struct node * n) { //main parse function that calls 
    int a = parse_lparen(n);
    if (a == 1)
        return 0;
    int b = parse_num(n); //other small parse functions for specific types 
    if (b == 1)
        return 0;
    int c = parse_op(n);
    if (c == 1)
        return 0;
    int d = parse_num(n);
    if (d == 1)
        return 0;
    int e = parse_rparen(n);
    if (e == 1) 
        return 0;
    parse(n);
} 

void display_op(int n) { //print out the type of operation based on the previously 
    switch (n) {         //assigned value
        case 1: 
        printf("add\n");
        break;
        case 2: 
        printf("sub\n");
        break;
        case 3: 
        printf("mul\n");
        break;
        case 4: 
        printf("div\n");
        break;
        }
}

void display(struct node * n) {
    if (n->right != NULL)
        display(n->right);
    if (n->left != NULL)
        display(n->left);
    if (n->op == 0)
        printf("push %d\n", n->num);
    else 
        display_op(n->op);
    if (n->previous && (n->previous->op != 8)) //if n->previous exists and it is not root node
    {
        struct node * temp = n->previous;
        free(n);
        n = temp;
    }
}



int main() { 
    struct node* root= malloc (sizeof(struct node));
    struct node* n = root;
    root->op = 8; //assign a random number to distinguish root->op
    printf("Welcome to Vincent's Compiler!\n\n");
    printf("Please start pressing buttons:\n ");
    int c = getchar();
    if (c != '(')
        ungetc(c, stdin);
    parse(n);  //call the main parse function to parse the user input
    if (n != root) {
        printf("Parenthesis don't match!");
        exit(EXIT_SUCCESS);
    }
    printf("--------------\n");
    display(n); //print out the compiled language from the root  
    printf("done\n");
    return 0;
}
