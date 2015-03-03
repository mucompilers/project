#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LENGTH 1024

typedef struct node {
	char* value;
	struct node* parent;
	struct node* lchild;
	struct node* rchild;
} node;

char getchar_nospaces();
int read_input(char* input, int* length);
int check_input(char* input, int length);
node* build_tree(char* input, int length);
void eval_tree(node* head);
void free_tree(node* head);

int main (void) {
	printf("djb8tc - Arithmetic Expression Compiler\nUse Ctrl+C to quit.\n");
	char input[MAX_LENGTH]; //Hopefully we don't encounter an abnormally long expression...
	int length;
	while (1) {
		if (read_input(input, &length)) { //Check for read errors
			while (getchar() != '\n'); //Clear expression with errors
			continue;
		}
		if (check_input(input, length)) { //Check for syntax errors
			printf("Note: Error messages with index numbers are calculated using only non-space characters.\n");
			while (getchar() != '\n'); //Clear expression with errors
			continue;
		}
		node* ast = build_tree(input, length);
		eval_tree(ast);
		free_tree(ast);
		printf("Done\n");
	}
}

//Grabs only the characters that aren't spaces.
char getchar_nospaces() {
	char c;
	do {
		c = getchar();
	} while (c == ' ');
	return c;
}

int read_input(char* input, int* length) {
	char c = getchar_nospaces();
	int index = 0;
	while (c != '\n') {
		if (index >= MAX_LENGTH) { //Length check
			printf("Error, expression too long. Please try again with a shorter expression.\n");
			return 1;
		}
		if (!isdigit(c) && c != '(' && c != ')' && c != '+' && c != '-' && c != '*' && c != '\\') { //Invalid character check
			printf("Error, invalid input. Please use only numbers, parentheses, spaces, and valid operators e.g. +-*\\\n");
			return 1;
		}
		input[index] = c;
		++index;
		c = getchar_nospaces();
	}
	*length = index;
	return 0;
}

int check_input(char* input, int length) {
	//Putting error checking for leading and closing parentheses here to simplify switch later.
	int index;
	int flag = 0;
	for (index = 0; index < length; ++index) { //Checking if all are numbers, then parentheses don't matter.
		if (!isdigit(input[index])) {
			flag = 1;
			break;
		}
	}
	if (flag == 0) { //If all characters are numbers, return successfully.
		return 0;
	}
	if (input[0] != '(') {
		printf("Error, invalid expression syntax: First character not LPAREN. Please try again.\n");
		return 1;
	}
	if (input[length - 1] != ')') {
		printf("Error, invalid expression syntax: Last character not RPAREN. Please try again.\n");
		return 1;
	}
	int lparencount = 1;
	int rparencount = 1;
	int opcount = 0;
	for (index = 1; index < length - 1; ++index) { //Bounds moved in by one as they're guaranteed to be LPAREN and RPAREN
		switch (input[index]) { //All syntax checking. Check error messages for details for what parts check for what.
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				if (input[index - 1] == ')') {
					printf("Error, invalid expression syntax: Missing operator between NUM and RPAREN around index %d. Please try again.\n", index);
					return 1;
				}
				if (input[index + 1] == '(') {
					printf("Error, invalid expression syntax: Missing operator between NUM and LPAREN around index %d. Please try again.\n", index);
					return 1;
				}
				if (input[index - 1] == '(' && input[index + 1] == ')') {
					printf("Error, invalid expression syntax: Invalid expression inside parentheses around index %d. Please try again.\n", index);
					return 1;
				}
				if (!isdigit(input[index - 1]) && input[index - 1] != '(' && input[index - 1] != ')' && !isdigit(input[index + 1]) && 
				input[index + 1] != '(' && input[index + 1] != ')') {
					printf("Error, invalid expression syntax: Enclosing parentheses missing for an expression around index %d. Please try again.\n", index);
					return 1;
				}
				break;
			case '(':
				if (input[index - 1] == ')' || isdigit(input[index - 1])) {
					printf("Error, invalid expression syntax: Missing operator before LPAREN around index %d. Please try again.\n", index);
					return 1;
				}
				if (input[index + 1] != '(' && !isdigit(input[index + 1])) {
					printf("Error, invalid expression syntax: Missing or invalid expression after LPAREN around index %d. Please try again.\n", index);
					return 1;
				}
				++lparencount;
				break;
			case ')':
				if (input[index - 1] != ')' && !isdigit(input[index - 1])) {
					printf("Error, invalid expression syntax: Missing or invalid expression before RPAREN around index %d. Please try again.\n", index);
					return 1;
				}
				if (input[index + 1] == '(' || isdigit(input[index + 1])) {
					printf("Error, invalid expression syntax: Missing operator after RPAREN around index %d. Please try again.\n", index);
					return 1;
				}
				++rparencount;
				break;
			case '+':
			case '-':
			case '*':
			case '\\':
				if (input[index - 1] != ')' && !isdigit(input[index - 1])) {
					printf("Error, invalid expression syntax: Missing NUM or expression before operator around index %d. Please try again.\n", index);
					return 1;
				}
				if (input[index + 1] != '(' && !isdigit(input[index + 1])) {
					printf("Error, invalid expression syntax: Missing NUM or expression after operator around index %d. Please try again.\n", index);
					return 1;
				}
				++opcount;
				break;
			default:
				printf("Error, invalid character detected. Please try again.\n");
				return 1;
		}
	}
	if (rparencount != lparencount) {
		printf("Error, invalid expression syntax: Unclosed or unopened parenthesis detected. Please try again.\n");
		return 1;
	} else if (lparencount != opcount) {
		printf("Error, invalid expression syntax: Number of parentheses do not match number of expressions, use one set per expression. Please try again.\n");
		return 1;
	}
	return 0;
}

//Represent input as an abstract syntax tree
node* build_tree(char* input, int length) {
	int i;
	//Create head
	node* head = (node*)malloc(sizeof(node));
	head->value = NULL;
	head->parent = NULL;
	head->lchild = NULL;
	head->rchild = NULL;
	node* current = head;
	for (i = 0; i < length; ++i) {
		if (input[i] == '(') { //Signals new subexpression, and so creates the needed nodes
			current->lchild = (node*)malloc(sizeof(node));
			current->rchild = (node*)malloc(sizeof(node));
			current->lchild->value = NULL;
			current->lchild->parent = current;
			current->lchild->lchild = NULL;
			current->lchild->rchild = NULL;
			current->rchild->value = NULL;
			current->rchild->parent = current;
			current->rchild->lchild = NULL;
			current->rchild->rchild = NULL;
			current = current->lchild;
		} else if (isdigit(input[i])) { //Signals number at leaf node, so insert and back up tree as needed
			int j = i;
			while (j < length && isdigit(input[j])) {
				++j;
			}
			current->value = (char*)malloc(sizeof(char) * (1 + j - i));
			int k;
			int l = 0;
			for (k = i; k < j; ++k) {
				current->value[l++] = input[k];
			}
			i = j - 1; //correcting for multiple digits inserted
			if (current == current->parent->lchild) {
				current = current->parent;
			} else {
				do {
					current = current->parent;
				} while (current->parent != NULL && current->value != NULL);
			}
		} else if (input[i] == ')') {
			//Do nothing, the RPAREN is worthless for tree building.
		} else { //Only operators left, so insert and go to rchild for their second argument
			current->value = (char*)malloc(sizeof(char)); 
			*(current->value) = input[i];
			current = current->rchild;
		}
	}
	return head;
}

void eval_tree(node* head) { //Simple postorder traversal from right to left.
	if (head == NULL) {
		return;
	}
	eval_tree(head->rchild);
	eval_tree(head->lchild);
	if (isdigit(head->value[0])) {
		printf("Push %s\n", head->value);
	} else {
		switch (*(head->value)) {
			case '+':
				printf("Add\n");
				break;
			case '-':
				printf("Sub\n");
				break;
			case '*':
				printf("Mul\n");
				break;
			case '\\':
				printf("Div\n");
				break;
		}
	}
}

void free_tree(node* head) { //Free allocated memory.
	if (head == NULL) {
		return;
	}
	free(head->value);
	free_tree(head->lchild);
	free_tree(head->rchild);
	free(head);
}