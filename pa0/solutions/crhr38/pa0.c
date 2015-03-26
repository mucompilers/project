#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#define MAX_DIGITS 9

struct ast {
      struct ast* left;
      struct ast* right;
      char num[MAX_DIGITS + 1];
      char op;
};

static struct ast* parse(void);
static void parse_space(void);
static struct ast* parse_num(void);
static struct ast* parse_exp(void);

static void print_sml(struct ast*);
static void destroy_ast(struct ast*);

int main(void) {
      struct ast* ast = parse();

      if (ast) {
            print_sml(ast);
            puts("done");
            destroy_ast(ast);
      } else {
            puts("Error during parsing!");
      }
}

struct ast* parse(void) {
      parse_space();

      struct ast* node = parse_num();
      parse_space();

      if (node) return node;

      node = parse_exp();
      parse_space();

      return node;
}

void parse_space(void) {
      int c;
      for (c = getchar(); c && isspace(c); c = getchar());
      ungetc(c, stdin);
}

struct ast* parse_num(void) {
      char num[MAX_DIGITS + 1] = {0};

      int c = getchar();
      for (int i = 0; i != MAX_DIGITS && isdigit(c); ++i, c = getchar()) {
            num[i] = (char)c;
      }

      if (c != EOF) ungetc(c, stdin);

      if (isdigit(c) || !num[0])
            return NULL;

      struct ast* ast = malloc(sizeof(*ast));
      ast->op = 0;
      strcpy(ast->num, num);

      return ast;
}

struct ast* parse_exp(void) {
      if (getchar() != '(') return NULL;

      struct ast* left = parse();
      if (!left) return NULL;

      int op = getchar();
      if (op != '+' && op != '-' && op != '*' && op != '\\')
            return NULL;

      struct ast* right = parse();
      if (!right) return NULL;

      if (getchar() != ')') return NULL;

      struct ast* exp = malloc(sizeof(*exp));

      exp->left = left;
      exp->op = (char)op;
      exp->right = right;

      return exp;
}

void print_sml(struct ast* ast) {
      assert(ast);

      if (ast->op) {
            print_sml(ast->right);
            print_sml(ast->left);

            switch (ast->op) {
                  case '+':
                        puts("add");
                        break;
                  case '-':
                        puts("sub");
                        break;
                  case '*':
                        puts("mul");
                        break;
                  case '\\':
                        puts("div");
            }

      } else {
            printf("push %s\n", ast->num);
      }
}

void destroy_ast(struct ast* ast) {
      assert(ast);

      if (ast->op) {
            destroy_ast(ast->right);
            destroy_ast(ast->left);
      }
      free(ast);
}
