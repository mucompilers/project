//
//  main.c
//  Tokenizer
//
//  Created by Qianli Zhang on 1/26/15.
//  Copyright (c) 2015 Qianli Zhang. All rights reserved.
//

#include <stdio.h>
#include "string.h"

#define  maxsize 100
#define  false 0
#define  true  1
char input[maxsize];
int length;

char output[maxsize];
char outputPtr;

char stack[maxsize];
int top;

int amountOfNumber;




/*
 below is a stack;
 method:
 int push(char ch);
 char pop();
 int isEmpty();
 */
int push(char ch){
    top++;
    if(top>=maxsize)
        return  false;
    stack[top] = ch;
    return  true;
}
char pop(){
    if(top<0||top>maxsize)
        return '#';
    return stack[top--];
}
int isEmpty(){
    return top==-1;
}
/*
 define whether a char a operator
 */
int Operator(char ch){
    if(ch=='+'||ch=='-'||ch=='*'||ch=='\\'||ch=='#')
        return 1;
    else return 0;
}
// state mechine
enum STATE{
    BEGIN,
    NUMBER,
    ARTHIMETIC,//end with ',' not ';'
    LPARENTHESES,
    RPARENTHESES,
    END
};
enum STATE state;

int DoTrans(){
    int outputState = 1;
    int LParent[maxsize];
    int LParentPtr = -1;
    char onThis;//char of input string
    char onTop;//on top of stack
    int i = length-1;
    int counter = 0;
    state = BEGIN;
    for(; i>=0; i--){
        onThis = input[i];
        if(onThis == ' ')
            continue;
        switch (onThis) {
            case ')':
                
                LParent[++LParentPtr] = -(i+1);
                if(state == NUMBER){
                    printf("[ERROR] No operator between a number and a ')'\n");
                    outputState = 0;
                    // 下面这部分没用
                    if(counter>9){
                        printf("[ERROR] the number ");
                        outputPtr = outputPtr -1;
                        for(;;outputPtr--){
                            if(output[outputPtr]<48||output[outputPtr]>57)
                                break;
                            printf("%c",output[outputPtr]);
                        }
                        printf(" is more than 9-digits\n");
                        outputState = 0;
                    }
                    counter=0;
                }
                
                push(onThis);
                state = RPARENTHESES;
                
                break;
                
            case '(':
                if(LParent[LParentPtr]<0)
                    LParentPtr--;
                else
                    LParent[++LParentPtr] = i+1;
                if(state == NUMBER){
//                    printf("[ERROR] No operator between a number and a LPARENT\n");
//                    outputState = 0;
                    if(counter>9){
                        printf("[ERROR] the number ");
                        outputPtr = outputPtr -1;
                        for(;;outputPtr--){
                            if(output[outputPtr]<48||output[outputPtr]>57)
                                break;
                            printf("%c",output[outputPtr]);
                        }
                        printf(" is more than 9-digits\n");
                        outputState = 0;
                    }
                    counter=0;
                }
                
                onTop = pop();
                int stackState = 0;
                while(!isEmpty()){
                    if(onTop==')')
                        break;
                    
                    output[outputPtr] = onTop;
                    outputPtr++;
                    state = LPARENTHESES;
                    onTop = pop();
                }
//                if(isEmpty() && onTop!=')'){
//                    printf("[ERROR] The '(' at the location ");
//                    printf("%d",i);
//                    printf(" has no pair\n");
//                    outputState = 0;
//                }
                state = LPARENTHESES;
                break;
                
            case '+':
            case '-':
            case '*':
            case '\\':
                
                if(state == NUMBER){
                    if(counter>9){
                        printf("[ERROR] the number ");
                        outputPtr = outputPtr -1;
                        for(;;outputPtr--){
                            if(output[outputPtr]<48||output[outputPtr]>57)
                                break;
                            printf("%c",output[outputPtr]);
                        }
                        printf(" is more than 9-digits\n");
                        outputState = 0;
                    }
                    counter=0;
                    //return outputState;
                }
                if(state == ARTHIMETIC){
                    printf("[ERROR] No number between two  ARTHIMETICS\n");
                    outputState = 0;
                }
                
                push(onThis);
                state = ARTHIMETIC;
                break;
                
            default:
                if(onThis<48||onThis>57){
                    printf("[ERROR] Unknown Charactor: ");
                    printf("%c\n", onThis);
                    outputState = 0;
                }
                if(state == LPARENTHESES){
                    printf("[ERROR] No operator between a number and a '('\n");
                    outputState = 0;
                }
                if(state!=NUMBER){
                    output[outputPtr] = '#';
                    outputPtr++;
                }
                output[outputPtr] = onThis;
                outputPtr++;
                counter++;
                state = NUMBER;
                break;
        }
    }
    if(!isEmpty()){
        printf("[ERROR] There should be PARENTHESES on both side of an operator\n");
        outputState = 0;
    }
    if(LParentPtr!=-1){
        for(int i=LParentPtr; i>=0;i--){
            char ch = LParent[i];
            if(ch>0){
                printf("[ERROR] The '(' at the location ");
                printf("%d",ch-1);
                printf(" has no pair\n");
            }else{
                printf("[ERROR] The ')' at the location ");
                printf("%d",-(ch-1));
                printf(" has no pair\n");
            }
        }
        outputState = 0;
    }
    return outputState;
}

int display(char ch){
    switch(ch){
        case '+':
            printf("add\n");
            break;
        case '-':
            printf("sub\n");
            break;
        case '*':
            printf("mul\n");
            break;
        case '\\':
            printf("div\n");
            break;
        case '#':
            printf("push ");
            break;
        default:
            printf("[ERROR]: unknown charactor: ");
            printf("%c\n",ch);
            return 0;
    }
    return 1;
}



int main(int argc, const char * argv[]) {
    while(1){
    top = -1;
    amountOfNumber = 0;
    outputPtr = 0;
    gets(input);
    length = strlen(input);
    
    if( !DoTrans()){
        printf("done\n");
        return 0;
    }
    length = strlen(output);
    int outputState = 0;
    top = -1;
    char temp;
    for(int i=0; i<length;i++) {
        if(Operator(output[i])){
            if(outputState==1){
                while(!isEmpty()){
                    temp = pop();
                    printf("%c",temp);
                }
                printf("\n");
            }
            display(output[i]);
            outputState = 0;
        }
        else{
            push(output[i]);
            outputState = 1;
        }
    }
    if(outputState==1){
        while(!isEmpty()){
            temp = pop();
            printf("%c",temp);
        }
        printf("\n");
    }
    printf("done\n");
    }
    return 0;
}
