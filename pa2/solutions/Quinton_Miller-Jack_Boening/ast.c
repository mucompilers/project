/*
 * CS 4430 - Compilers
 * Assignment Three
 * Quinton Miller
 * Jack Boening
 * */

#include "ast.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* PUSH :: Pushes new value to linked list */
List *push(List *head, AST *val){
 
  if (head == NULL){
    head = (List *) malloc( sizeof( List ) );
    head->next = NULL;
    head->this = val;
  }
  else{
    List *new, *it;
    new = (List *) malloc( sizeof( List ) );
    new->next = NULL;
    new->this = val;
    it = head;
    while (it->next)
      it = it->next;
    it->next = new;
  }
  
  return head;
  
}

AST *build_AST(char *val, List *children){
  
  AST *ret = (AST *) malloc( sizeof( AST ) );
  ret->val = malloc( sizeof( char ) * strlen( val ) );
  //memcpy(ret->val, val, sizeof(val));
  strcpy(ret->val, val);
  ret->children = children;
  return ret;
  
}

void print_AST(AST *head, int depth){
  int i;
  if (!head)
    return;
  if (!head->val){
    printf("ERROR");
    return;
  }
  for (i = 0; i < depth; i++)
  printf("\t");
  List *it = head->children;
  printf("(%s", head->val);
  if (it){
    while (it && it->this){
      printf("\n");
      print_AST(it->this, depth + 1);
      it = it->next;
    }
  }
  //for (i = 0; i < depth && head->children; i++)
    //printf("\t");
  printf(")");
  
}
