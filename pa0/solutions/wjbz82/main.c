/*
    Equation Verifier
    William Jack Boening
    Wjbz82
    14163911

    Compilers - Spring 2015 / Mizzou
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

typedef struct node_{
    char data;
    struct node_* next;
}node;
                            //Stack data structure; this was used to hold the operators that were found 
                            //inside the parenthesis, so that they could be printed out in order 
typedef struct stack_{
    int size;
    node* stack;
}stack;

stack* create_stack();
node* push(stack* s, char val);
void pop(stack* s);             //Function declerations
int getCaseInt(char c);
int verifyToken(int caseInt, int expectedTokens[2]);

int main(int argc, char* argv[])
{
    stack* s = create_stack();  //building main stack 
    int expectedTokens[3] = {6, 1, 1}; //this was how I approached solving the problem: 
                            //I created an array of "expected tokens", and for ecah token encountered,
                            //i set what was expected based on basic knowledge of the language we are creating this lexer / parser for
    char inputString[100];
    int i;
    int paren_count = 0;    //other initial variables
    char num[10];
    num[10] = '\0';
    int index = 9;

    printf("Enter an expression: ");
    fgets(inputString, sizeof(inputString), stdin); //asking the user for an expression to be entered from stdin

   size_t ln = strlen(inputString) - 1;
   if(inputString[ln] == '\n')      //for some reason, the enter key was counted when inputting the string, so I remove the newline char, and set it to the null terminator
        inputString[ln] = '\0';

    /*
        For this loop here, we loop from i = the length of the input string, down to 0
        And for each iteration, we get the caseInt (which is basically a integer representation of the current token we are inspecting; see function below)
        Then, after we have that token, we must verify that the token we are currently on matches what is expected; and do that we call a verifier function to make sure that the current case token is present inside the expected tokens array
        If that turns out to be true, then we do a switch statement on the case token. 

        Based upon what the case token is, we have several cases
        0 = LPAREN
        1 = RPAREN
        2 - 5 = OPERATOR
        6 = NUMBER
        7 = SPACE
        8 = INVALID CHARACTERS
    */

    for(i = strlen(inputString) - 1; i >= 0; i--){  
        int caseInt = getCaseInt(inputString[i]);
        int verifiedResult = verifyToken(caseInt, expectedTokens);
        if(verifiedResult == 0){
            switch(caseInt){
            case 0:
                pop(s);             //inside case 0; LPAREN, we want to first call pop and pop whatever operators are still on the stack, and this make sense to do because the only time we would want to apply an operator is at the end of an expression
                paren_count--; // using a variable called paren_count, I keep track of how many parens there are in the input string, and if it falls below 0, meaning there is an imbalance, I exit the program and alert the user of this
                if(paren_count < 0){
                    fprintf(stderr, "Error, this cannot possibly happen.\n");
                    exit(1);
                }
                if(paren_count == 0){
                    break;              //if the paren_count = 0, we can assume the equation is finished, otheriwse there would be some kind of imbalance between them 
                }

                expectedTokens[1] = 0;  //we then set the expected tokens to be another LPAREN, or any given operator
                expectedTokens[2] = 9;
                break;
            case 1:
                //in the RPAREN case, we only have to increment the paren_count and set the expected tokens
                expectedTokens[0] = 1;   //which are another RPAREN, any NUMBER, or a LPAREN
                expectedTokens[1] = 6;
                expectedTokens[2] = 0;
                paren_count++;
                break;
            case 2:
            case 3:
            case 4:
            case 5:

                //cases 2-5 are all the same, since it is for any given operator 
                //all we do is set the expected tokens to be any NUMBER, RPAREN, or SPACE

                expectedTokens[0] = 6;
                expectedTokens[1] = 1;
                expectedTokens[2] = 7;
                s->stack = push(s, inputString[i]); //here we set the linked list "stack" inside of my stack data structure equal to itself with the new operator pushed on top
                break;
            case 6:

                /*
                    For the case of encountering a number, I first loop backwards through the string, and at each iteration, I make sure that I'm stil dealing with a digit (hence the while loop)
                    Then, I have a secondary array to temporarily hold the values that I find, and I store them at the end of the secondary array. I do it this way because if I'm reading from right to left, 
                    and I encounter the numbers in reverse order, I need to save them into the array in reverse order; so I start at the end of the array and go backwards through the array saving the number at each step

                    Then after I save it to the array, I increase the index of my array by 1 to get back to the starting number, otherwise it will print a space first and I print out the number that I just saved to the array
                    This is done by starting at the current index, and going until strictly less than 10 (this was specified that no number would be bigger than 9 characters) and print it out at each step. 
                    Then I print a newline for formatting concerns.

                    After that I set the expected tokens to a SPACE, LPAREN, and any OPERATOR
                    Finally, I reset the index of my secondary array back to the end, and increase my i counter by one, so the index of my input string is back on the proper place and is expecting what it I have it set to be expecting
                    (That last bit may be a bit confusing; but what happens is in my while loop I use the same index to go backwards that I'm using to loop through the entire string, and since I want to treat the entire number as one token, I need to put i back onto the last digit before the space: example (+ 755),
                    after reading this in with the while loop, the i will be on the SPACE in the main string, so I push it back to the 7, so it's truly expecting what it needs to)
                */

                while(isdigit(inputString[i])){
                    num[index] = inputString[i];
                    index--;
                    i--;
                }
                index++;
		printf("push "); 
                for(index; index < 10; index++){
                    printf("%c", num[index]);
                }
                printf("\n");
                expectedTokens[0] = 7;
                expectedTokens[1] = 0;
                expectedTokens[2] = 9;
                i++;
                index = 9;
                break;
            case 7:
                //this is the space case, since we are ignoring spaces, I just break and move on to the next character without updating what is expected
                break;
            case 8:
                //this is what is encountered if an invalid character is found during reading of the string
                fprintf(stderr, "Error, invalid character encountered.\n");
                exit(1);
                break;
            }
        }else{
            //this is what happens if the first expression read in is invalid, I only want expressions that start with a ) or a number
            fprintf(stderr, "Error, invalid expression input.\n");
            exit(1);
        }
    }
    //printing done after the epxression is finished
    printf("DONE\n"); 
    return 0;
}

/*
    This function takes in a current integer token representation of the current token and the expected tokens array
    then it sees that if the current integer token is any of the operators, and if any part of the expected tokens is a 9 (meaning that any operator is expected) I return a 0 (true)

    Otherwise, I check for each index in the expected tokens array, that if it matches the caseInt, I return 0 (true); otherwise I return 1 (false)
*/

int verifyToken(int caseInt, int expectedTokens[2]){

    if((caseInt == 2 || caseInt == 3 || caseInt == 4 || caseInt == 5) && expectedTokens[2] == 9){
        return 0;
    }

    if(caseInt == expectedTokens[0]){
        return 0;
    }else if(caseInt == expectedTokens[1]){
        return 0;
    }else if(caseInt == expectedTokens[2]){
        return 0;
    }else{
        return 1;
    }
}

/*
    This is just the funcation mapping for the current language, since only a selection of characters are allowed, mapping the different cases are a straight forward thing to do
*/

int getCaseInt(char c){
    if(c == '('){
        return 0;
    }
    if(c == ')'){
        return 1;
    }
    if(c == '+'){
        return 2;
    }
    if(c == '-'){
        return 3;
    }
    if(c == '/' || c == '\\'){
        return 4;
    }
    if(c == '*'){
        return 5;
    }
    if(isdigit(c)){
        return 6;
    }
    if(isspace(c)){
        return 7;
    }
    return 8;
}

//this function creates a stack to hold the operators as they are encountered 
stack* create_stack(){
    stack* s;
    s = malloc(sizeof(stack));
    s->size = 0;
    s->stack = NULL;
    return s;
}

//this function pushes the current val (operator) to top of the stack and returns the updated stack back to the function caller 
node* push(stack* s, char val){
    node* newNode = malloc(sizeof(node));
    newNode->data = val;
    newNode->next = s->stack;
    s->size = s->size + 1;
    return newNode;
}

/*
    This function is called every time a LPAREN is met, and it first makes sure that the linked list (stack) is not empty, so I set a character variable c equal to the current stack data 
    Otherwise, we alret the user that the stack is empty (meaning no operators were pushed to stack)

    Then we do a set of if statements to see what the character variable is equal to; 
    if it's a +, we print ADD
    - = SUB
    / or \ = DIV
    * = MUL

    Then we remove that element from the linked list and set the stack pointer to the next element in the list
*/

void pop(stack* s){

    char c;

    if(s->stack){
        c = s->stack->data;
    }else{
        fprintf(stderr, "ERROR: stack is empty\n");
        return;
    }

    if(c == '+')
        printf("add\n");
    if(c == '-')
        printf("sub\n");
    if(c == '/' || c == '\\')
        printf("div\n");
    if(c == '*')
        printf("mul\n");


    node* temp = s->stack;
    s->stack = s->stack->next;
    free(temp);
    return;
}
