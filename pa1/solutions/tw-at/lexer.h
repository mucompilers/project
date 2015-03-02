#ifndef LEXER_H_
#define LEXER_H_

#include <stdlib.h>
#include <stdio.h>

enum {

      NUM = 256, ID, LITCHAR, LITSTR, LITDEC, LITBOOL,

/*Ambigous Lexemes*/
	  ASTERISK/*MUL operator or DEREFERENCE*/, 
	  NEGATIVE_SIGN /*LOGICAL_NEGATE or SUB*/,
      AS,/*as operator or as keyword*/

/*Unary Operators*/ //precedence all equal (these have higher than binary operations)
	  LOGICAL_NEGATE,
 
/*Binary Operators*/ //precedence left to right descending
      DIV, REM, ADD, SHL, SHR, 
	  BITAND, BITXOR, BITOR, EQ, NE, LT, GT, LE, GE,
	  BOOL_AND, BOOL_OR, ASSIGNMENT, RANGE,

/*Compound Binary Operators*/
	  PLUS_ASSIGN, SUB_ASSIGN, MUL_ASSIGN, DIV_ASSIGN,
	  REM_ASSIGN, BITAND_ASSIGN, BITXOR_ASSIGN, BITOR_ASSIGN,
	  SHL_ASSIGN, SHR_ASSIGN,

/*Symbols*/
	  PATH, FUNCTION_ARROW, MATCH_ARROW, DIRECTIVE, DIRECTIVE_FEATURE, APOST,
	  DOLLAR, LSQUARE, RSQUARE, LPAREN, RPAREN, LCURLY, RCURLY, TRIPLE_DOT, DOT, COMMA, COLON,
	  SEMICOLON, 

/*Keywords*/
	  ABSTRACT, ALIGNOF, BE, BOX,
	  BREAK, CASE, CONST, CONTINUE, CRATE, DO,
	  ELSE,	ENUM, EXTERN, FALSE, FINAL,
	  FN, FOR, IF, IMPL, IN,
	  LET, LOOP, MACRO, MACRO_RULES, MATCH, MOD,
	  MOVE,	MUT, OFFSETOF, OVERRIDE, PRIV,
	  PUB, PURE, REF, RETURN, SIZEOF,
	  STATIC, SELF, STRUCT, SUPER, TRUE,
      TRAIT, TYPE, TYPEOF, UNSAFE, UNSIZED,
	  USE, VIRTUAL, WHERE, WHILE, YIELD,

/*Built-in Datatypes*/
	  UNIT, BOOL, U8, U16, U32, U64, I8, I16,
	  I32, I64, F32, F64, USIZE, ISIZE, CHAR,
	  STR,

      UNKNOWN
};


#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif


#ifndef YY_STRUCT_YY_BUFFER_STATE
#define YY_STRUCT_YY_BUFFER_STATE
struct yy_buffer_state
	{
	FILE *yy_input_file;

	char *yy_ch_buf;		/* input buffer */
	char *yy_buf_pos;		/* current position in input buffer */

	/* Size of input buffer in bytes, not including room for EOB
	 * characters.
	 */
	yy_size_t yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	int yy_n_chars;

	/* Whether we "own" the buffer - i.e., we know we created it,
	 * and can realloc() it to grow it, and should free() it to
	 * delete it.
	 */
	int yy_is_our_buffer;

	/* Whether this is an "interactive" input source; if so, and
	 * if we're using stdio for input, then we want to use getc()
	 * instead of fread(), to make sure we stop fetching input after
	 * each newline.
	 */
	int yy_is_interactive;

	/* Whether we're considered to be at the beginning of a line.
	 * If so, '^' rules will be active on the next match, otherwise
	 * not.
	 */
	int yy_at_bol;

    int yy_bs_lineno; /**< The line count. */
    int yy_bs_column; /**< The column count. */
    
	/* Whether to try to fill the input buffer when we reach the
	 * end of it.
	 */
	int yy_fill_buffer;

	int yy_buffer_status;

#define YY_BUFFER_NEW 0
#define YY_BUFFER_NORMAL 1
	/* When an EOF's been seen but there's still some text to process
	 * then we mark the buffer as YY_EOF_PENDING, to indicate that we
	 * shouldn't try reading from the input source any more.  We might
	 * still have a bunch of tokens to match, though, because of
	 * possible backing-up.
	 *
	 * When we actually see the EOF, we change the status to "new"
	 * (via yyrestart()), so that the user can continue scanning by
	 * just pointing yyin at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */



#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif




char* yytext;
int yylex(void);



#endif
