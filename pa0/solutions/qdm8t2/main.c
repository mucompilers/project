//Name: Quinton D Miller
//Pawpring: QDM8T2
//Date: Feb 02, 2015
//Assignment: 0
//Class: CS4430

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

struct AST{
  char oper;          // operator symbol
  int value;          // number value
  int lastChar;       // position of last character
  struct AST* left;   // left branch
  struct AST* right;  // right branch
};

//function prototypes
char* oper_string(char oper);
struct AST* create_node(char in[], int i);
int get_num(char in[], int i);
void print_tree(struct AST* head);
void free_tree(struct AST* head);

//MAIN
//takes in string and expects format specified by assignment
//if correct format, will output stack requests to solve expression
//if incorrect, will output error
int main(int argc, char *argv[]){

  //vars
  char input[255];
  int i = 0;
  struct AST *head = NULL;

  //get input
  if (argc == 1){
    printf("Please enter input: ");
    fgets(input, sizeof(input), stdin);
    strtok(input, "\n");
  }
  else{
    strcpy(input,*(argv+1));
  }

  //print input
  //printf("Input: %s\n", input);

  //create AST
  head = create_node(input, 0);

  //if AST was successfully created
  if (head){

    //if trailing character after last ) or last digit
    if (head->lastChar < 255 && input[head->lastChar + 1]){
      printf("Error: Invalid syntax\n");
      free_tree(head);
      return 1;
    }

    //print and free
    print_tree(head);
    printf("done");
    free_tree(head);
  }
  //if Error (error message printed out already)
  else
    return 1;

  //complete program
  return 0;
}

//OPERATOR STRING
//takes in operator id
//returns operator string
char* oper_string(char oper){
  switch(oper){
  case '*':
    return "mul";
  case '\\':
    return "div";
  case '+':
    return "add";
  case '-':
    return "sub";
  }
  return "Error: Not a valid operator id\n";
}


//CREATE NODE
//takes in char array and starting point
//recursively creates the AST
struct AST* create_node(char in[], int i){

  //create AST
  struct AST* ret = malloc(sizeof(struct AST));

  //skip to first char
  while (in[i] == ' ')
    i++;

  //if first char is open paren
  if (in[i] == '('){

    //get left node
    ret->left = create_node(in,i + 1);
    if (!ret->left){
      free(ret);
      return NULL;
    }

    i = ret->left->lastChar + 1;

    //get next non-space char
    while (in[i] == ' ')
      i++;

    //set operator
    if (in[i] == '+' || in[i] == '-' || in[i] == '*' || in[i] == '\\'){
      ret->oper = in[i];
      ret->value = -1;
    }
    else{
      printf("Error: Invalid syntax\n");
      free(ret);
      return NULL;
    }

    //get right node
    ret->right = create_node(in,i + 1);
    if (!ret->right){
      free(ret);
      return NULL;
    }

    i = ret->right->lastChar + 1;

    //get next non-space char
    while (in[i] == ' ')
      i++;

    //check if closing paren
    if (in[i] != ')'){
      printf("Error: Invalid syntax\n");
      free(ret);
      printf("[%c]",in[i]);
      return NULL;
    }
    else{
      ret->lastChar = i;
    }
  }
  //or if a number
  else if (isdigit(in[i])){
    ret->oper = 0;
    ret->left = NULL;
    ret->right = NULL;
    ret->value = get_num(in,i);
    if (ret->value < 0){
      printf("Error: Invalid Syntax\n");
      free(ret);
      return NULL;
    }
    while (isdigit(in[i]))
      i++;
    ret->lastChar = i - 1;
  }
  else{
    printf("Error: Invalid syntax\n");
    free(ret);
    return NULL;
  }

  return ret;
}

//GET NUMBER
//takes in char array and starting point
//returns max 9-digit char on success
//        -1 on fail
int get_num(char in[], int i){

  //variables
  int j = 0;
  char num[10] = {' ',' ',' ',' ',' ',' ',' ',' ',' ', '\0'};

  //loop thru number
  while (isdigit(in[i]) && j < 9){
    num[j] = in[i];
    i++;
    j++;
  }

  //check if less than or equal to 9 digits
  if (isdigit(in[i++])){
    printf("Error: Exceeded 9 digit maximum per number\n");
    return -1;
  }

  //return number
  return atoi(num);
}

//PRINT TREE
//takes in AST head
//prints tree in correct order for stack (right, left, head)
void print_tree(struct AST* head){
  if (head){
    //print right branch
    print_tree(head->right);

    //print left branch
    print_tree(head->left);

    //print self
    if (head->oper){
      printf("%s\n", oper_string(head->oper));
      return;
    }
    if (head->value >= 0){
      printf("push %d\n", head->value);
      return;
    }

    printf("Error\n");
  }
}

//FREE TREE
//takes in tree head
//frees children then self
void free_tree(struct AST* head){
  if (head){
    free_tree(head->right);
    free_tree(head->left);
    free(head);
  }
}
