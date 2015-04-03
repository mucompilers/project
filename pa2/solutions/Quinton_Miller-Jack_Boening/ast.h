/*
 *  * CS 4430 - Compilers
 *   * Assignment Three
 *    * Quinton Miller
 *     * Jack Boening
 */
#ifndef AST_H_
#define AST_H_

typedef struct ast{
  char *val;
  struct list *children;
}AST;

typedef struct list{
  AST *this;
  struct list *next;
}List;

List *push(List *head, AST *val);
AST *build_AST(char *val, List *children);
void print_AST(AST *head, int depth);

#endif
