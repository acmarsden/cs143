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

bool str_length_reached(){
	return string_buf_ptr - &string_buf[0] >= MAX_STR_CONST-1;
}

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>

CLASS        (?i:class)
ELSE         (?i:else)
FI           (?i:fi)
IF           (?i:if)
INHERITS     (?i:inherits)
IN           (?i:in)
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
NOT	     (?i:not)
STR_CONST    [a-zA-Z0-9\ \_\\]*
INT_CONST    [0-9]+
BOOL_CONST   t(?i:rue)|f(?i:alse)
TYPEID 	     [A-Z][a-zA-Z0-9\_]*
OBJECTID     [a-z][a-zA-Z0-9\_]*
ASSIGN	     <-
LE	     <=
WHITESPACE   [ \n\f\t\v\r]+
%x STAR_COMMENT
%x DASH_COMMENT
%x STR
%x ENDSTR
%x STRERR

%%

 /*
  *  Nested comments
  */

 /*
  *  The multiple-character operators.
  */

{DARROW} 		{ return(DARROW); }
{CLASS}			{ return(CLASS); }
{ELSE}			{ return(ELSE); }
{FI}			{ return(FI); }
{IF} 			{ return(IF); }
{IN}/{WHITESPACE}	{ return(IN); }
{INHERITS}		{ return(INHERITS); }
{LET} 			{ return(LET); }
{LOOP}			{ return(LOOP); }
{POOL}			{ return(POOL); }
{THEN}			{ return(THEN); }
{WHILE}			{ return(WHILE); }
{CASE}			{ return(CASE); }
{ESAC}			{ return(ESAC); }
{OF}			{ return(OF); }
{NEW}			{ return(NEW); }
{ISVOID}		{ return(ISVOID); }
{NOT}			{ return(NOT);}
{BOOL_CONST}		{ char* yytext_start = yytext;
			  for ( ; *yytext; ++yytext) *yytext = tolower(*yytext);
			  yytext = yytext_start;
			  yylval.boolean = strcmp(yytext,"false");
			  return(BOOL_CONST);
			}
\"                      { /* Found string beginning character*/
			  string_buf_ptr = string_buf;
			  BEGIN(STR); }
<STR,ENDSTR>\"		{ /* Found string termination character*/
			  BEGIN(INITIAL);
			  *string_buf_ptr = '\0';
			  yylval.symbol = stringtable.add_string(string_buf);
			  return(STR_CONST);}
<STRERR>[^\"\n]*\n	{ /* Eat up rest of the string */
			  ++curr_lineno;
			}
<STRERR>.*\"		{ /* Eat up rest of the string */
			  BEGIN(INITIAL);
			  return(ERROR);
			}
<ENDSTR>\0	        { /* A null character was found */
			  yylval.error_msg = "String contains null character.";
			  BEGIN(STRERR);
			}
<STR,ENDSTR>\n		{ /* error - unterminated string constant */
			  ++curr_lineno;
			  yylval.error_msg = "Unterminated string constant";
			  BEGIN(STRERR);
			}
<STR,ENDSTR>\\n		{ if(str_length_reached()){
			  	yylval.error_msg = "String constant too long";
			  	BEGIN(STRERR);
			  }
			  *string_buf_ptr++ = '\n';
			  BEGIN(STR);
			}
<STR,ENDSTR>\\t		{ if(str_length_reached()){
			  	yylval.error_msg = "String constant too long";
			  	BEGIN(STRERR);
			  }
			  *string_buf_ptr++ = '\t';
			  BEGIN(STR);
			}
<STR,ENDSTR>\\r		{ if(str_length_reached()){
			  	yylval.error_msg = "String constant too long";
			  	BEGIN(STRERR);
			  }
			  *string_buf_ptr++ = '\r';
			  BEGIN(STR);
			}
<STR,ENDSTR>\\b		{ if(str_length_reached()){
			  	yylval.error_msg = "String constant too long";
			  	BEGIN(STRERR);
			  }
			  *string_buf_ptr++ = '\b';
			  BEGIN(STR);
			}
<STR,ENDSTR>\\f		{ if(str_length_reached()){
			  	yylval.error_msg = "String constant too long";
			  	BEGIN(STRERR);
			  }
			  *string_buf_ptr++ = '\f';
			  BEGIN(STR);
			}
<STR,ENDSTR>\\\0	{ if(str_length_reached()){
			  	yylval.error_msg = "String constant too long";
			  	BEGIN(STRERR);
			  }
			  yylval.error_msg = "String contains escaped null character.";
			  BEGIN(STRERR);
			}
<STR,ENDSTR>\\.		{ *string_buf_ptr++ = yytext[1];
			  BEGIN(STR);
			}
<STR,ENDSTR>\\\n	{ *string_buf_ptr++ = yytext[1];
			  ++curr_lineno;
			  BEGIN(STR);
			}

<STR>[^\\\n\"\0]+	{ char *yptr = yytext;
			  bool error = false;
			  while ( *yptr ){
				if(str_length_reached()){
			  		yylval.error_msg = "String constant too long";
					error = true;
					break;
				}
				*string_buf_ptr++ = *yptr++;
			  }
			  if(error){
			  	BEGIN(STRERR);
			  }else{
			  	BEGIN(ENDSTR);
			  }
			}
<STR><<EOF>>		{ yylval.error_msg = "EOF in string constant.";
			  BEGIN(INITIAL);
			  return(ERROR);
			}
{INT_CONST}		{ yylval.symbol = inttable.add_string(yytext);
			  return(INT_CONST); }
{TYPEID}		{ yylval.symbol = idtable.add_string(yytext);
			  return(TYPEID); }
{OBJECTID}		{ yylval.symbol = idtable.add_string(yytext);
			  return(OBJECTID); }
{ASSIGN}		{ return(ASSIGN); }
{LE}			{ return(LE); }
"(*"			{ BEGIN(STAR_COMMENT); }
"--"			{ BEGIN(DASH_COMMENT); }
<STAR_COMMENT,DASH_COMMENT>[^*\n]*	/* eat up comment content */
<STAR_COMMENT>"*)"		{ BEGIN(INITIAL);}
<DASH_COMMENT>.*/\n		{ BEGIN(INITIAL);}
<DASH_COMMENT><<EOF>>		{ BEGIN(INITIAL);}
<STAR_COMMENT><<EOF>>	{ yylval.error_msg = "EOF in comment";
			  BEGIN(INITIAL);
			  return(ERROR); }
"*)"			{ yylval.error_msg = "Unmatched *)";
			  return(ERROR); }
<INITIAL,STAR_COMMENT,DASH_COMMENT>\n	{ ++curr_lineno; }
:			{ return(':'); }
;			{ return(';'); }
[(]			{ return('('); }
[)]			{ return(')'); }
[{]			{ return('{'); }
[}]			{ return('}'); }
[.]			{ return('.'); }
[<]			{ return('<'); }
[=]			{ return('='); }
[+]			{ return('+'); }
[-]			{ return('-'); }
[,]			{ return(','); }
[ \f\t\r\v]+            /* eat up unused whitespace */ 
.			{ yylval.error_msg = yytext;
			  return(ERROR); }
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
