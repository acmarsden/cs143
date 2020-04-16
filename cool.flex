/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;



%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>

CLASS        (?i:class)
ELSE         (?i:else)
FI           (?i:fi)
IF           (?i:fi)
IN           (?i:in)
INHERITS     (?i:inherits)
LET          (?i:let)
LOOP         (?i:loop)
POOL         (?i:pool)
THEN         (?i:then)
WHILE        (?i:while)
CASE         (?i:case)
ESAC         (?i:esac)
OF           (?i:of)
NEW 	     (?i:new)
ISVOID	     (?i:isvoid)

STR_CONST    "."
INT_CONST    [0-9]+
BOOL_CONST   (T|t)/(?i:rue) | (F|f)/(?i:alse)

TYPEID	     [A-Z][a-zA-Z0-9]*
OBJECTID     [a-z][a-zA-Z0-9]*

%%

 /*
  *  Nested comments
  */

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{CLASS}			{ return (CLASS); }
{ELSE}			{ return (ELSE); }
{FI}			{ return (FI); }
{IF}			{ return (IF); }
{IN}			{ return (IN); }
{INHERITS}		{ return (INHERITS); }
{LET}			{ return (LET); }
{LOOP}			{ return (LOOP); }
{POOL}			{ return (POOL); }
{THEN} 			{ return (THEN); }
{WHILE}			{ return (WHILE); }
{CASE}			{ return (CASE); }
{ESAC} 			{ return (ESAC); }
{OF}			{ return (OF); }
{NEW}			{ return (NEW); }
{ISVOID}		{ return (ISVOID); }
{STR_CONST}		{ return (STR_CONST); }
{INT_CONST}		{ return (INT_CONST); }
 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
