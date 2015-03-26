/*
Brett Brusda
PA0 
14137988
*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define STACK 50
#define SIZE 15

//global variables
char s[STACK];
int top = -1;

void push(char token) {
    s[++top] = token;
}

char pop() {
    return (s[top--]);
}


int precendence(char token)
{                 
    switch(token)
    {
    case '#': return 0;
    case '(': return 1;
    case '+':
    case '-': return 2;
    case '*':
    case '/': return 3;
    }
}



//main
int main(void) {
    char infix[SIZE], postfix[SIZE], token, following_token, element;
    int i = 0, k = 0, j = 1;
    printf("Provide input: ");
	//store user input into infix array
    scanf("%s", infix);
	//push a terminating character onto the stack
    push('#');

    //check each individual token
    while ((token = infix[i++]) != '\0') {

        //push left parentheses onto stack
        if (token == '(') {
            push(token);
        }
        else {
            //verify token is a digit
            if (isdigit(token)) {
                following_token = infix[i];
                postfix[k++] = token;
				//check if following digit in numeric
                while (isdigit(following_token)) {
					//determine tokens for multiple digit numbers
                    postfix[k++] = following_token;
                    following_token = infix[i + j];
                    j++;
                    i++;
                }
				//put a separator to distinguish numbers
                postfix[k++] = '|';
            } 
			//check if token is equal to a slosing RPAREN
			else if (token == ')') {
                while (s[top] != '(') {
                    postfix[k++] = pop();
                }
                element = pop();
            } else {
                //prder of precendence for each operator
                while (precendence(s[top]) >= precendence(token)) {
                    postfix[k++] = pop();
                }
                push(token);
            }
        }
    }

    // loop through the stack until terminating character is reached
    while (s[top] != '#') {
        postfix[k++] = pop();
    }
	//add terminating character to array
    postfix[k] = '\0';
	printf("Postfix expression %s\n\n", postfix);
	printf("Stack form:\n");
    for (i = 0; postfix[i] != '\0'; i++) {
        token = postfix[i];
        if (!isdigit(token)) {
                switch (token) {
					case '-':
						printf("sub\n");
						break;
					case '+':
						printf("add\n");
						break;
					case '\\':
						printf("div\n");
						break;
					case '*':
						printf("mul\n");
						break;
					default:
						printf("Invalid token, program terminating\n");
						return 2;
				}
        } else {
			//print the digits
            printf("push ");
            while (isdigit((token = postfix[i]))) {
                printf("%c", token);
                i++;
            }
            printf("\n");
        }
    }

    printf("done\n");
    return 0;
}


