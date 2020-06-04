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
#include <string>

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

/*
 *  Add Your own definitions here
 */
int comment_depth = 0;

%}

/*
 * Define names for regular expressions here.
 */

DARROW    =>
LE        <=
ASSIGN    <-
TYPEID [A-Z][a-zA-Z0-9_]*
OBJECTID [a-z][_a-zA-Z0-9]*
CLASS    [cC][lL][aA][sS][sS]
ELSE     [eE][lL][sS][eE]
FI       [fF][iI]
IF       [iI][fF]
IN       [iI][nN]
INHERITS [iI][nN][hH][eE][rR][iI][tT][sS]
LET      [lL][eE][tT]
LOOP     [lL][oO][oO][pP]
POOL     [pP][oO][oO][lL]
THEN     [tT][hH][eE][nN]
WHILE    [wW][hH][iI][lL][eE]
CASE     [cC][aA][sS][eE]
ESAC     [eE][sS][aA][cC]
OF       [oO][fF]
NEW      [nN][eE][wW]
ISVOID   [iI][sS][vV][oO][iI][dD]
NOT      [nN][oO][tT]

%x COMMENT YYSTRING YYERROR YYLINECOMMENT COMMENTBLOCK

%%

 /*
  *  Nested comments
  */

<INITIAL,COMMENTBLOCK,YYLINECOMMENT>"(*" {
    comment_depth++;
    BEGIN(COMMENTBLOCK);
}

<COMMENTBLOCK>[^\n(*]* ;

<COMMENTBLOCK>[()*] ;

<COMMENTBLOCK>\n {
    curr_lineno++;
}

<COMMENTBLOCK>"*)" {
  comment_depth--;
  if (comment_depth == 0) BEGIN(0);
}

<COMMENTBLOCK><<EOF>> {
    yylval.error_msg = "EOF in comment";
    BEGIN(INITIAL);
    return ERROR;
}

"*)" {
    yylval.error_msg = "Unmatched *)";
    return ERROR;
}

 /*
  * line comment
  */
<INITIAL>"--"				 { BEGIN(YYLINECOMMENT); }
<YYLINECOMMENT>[^\n]*  {}
<YYLINECOMMENT>\n  { curr_lineno++; BEGIN(INITIAL);}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<INITIAL>(\") { BEGIN(YYSTRING); }

<YYSTRING>[^\\\"\n]* { yymore(); }
<YYSTRING>\\[^\n] { yymore(); }
<YYSTRING>\\\n { curr_lineno++; yymore(); }

<YYSTRING>\n {
  yylval.error_msg = "Unterminated string constant";
  BEGIN(INITIAL);
  curr_lineno++;
  return ERROR;
}

<YYSTRING><<EOF>> {
  yylval.error_msg = "EOF in string constant";
  BEGIN(INITIAL);
  yyrestart(yyin);
  return ERROR;
}

<YYSTRING>\" {
  BEGIN(INITIAL);
  // remove last \"
  std::string input(yytext,yyleng-1);

  if( input.find('\0') != std::string::npos ){
      yylval.error_msg = "String contains null character";
      return ERROR;
  }

  while(true)   {     
    std::string::size_type  pos(0);     
    if( (pos=input.find("\\b")) !=std::string::npos )     
      input.replace(pos,2,"\b");
    else if( (pos=input.find("\\t")) !=std::string::npos )     
      input.replace(pos,2,"\t");
    else if( (pos=input.find("\\n")) !=std::string::npos )     
      input.replace(pos,2,"\n");
    else if( (pos=input.find("\\f")) !=std::string::npos )     
      input.replace(pos,2,"\f");
    else
      break;     
  }

  /* escapse other char */
  std::string::size_type pos=0;
  while ((pos = input.find_first_of("\\",pos)) != std::string::npos) {
    input.replace(pos,2,input.substr(pos +1 , 1));
    pos++;
  }

  if(input.length() > MAX_STR_CONST-1 ){
    yylval.error_msg = "Unterminated string constant";
    return ERROR;
  }

  cool_yylval.symbol = stringtable.add_string((char*)input.c_str());
  return STR_CONST;
}


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS}      {return CLASS; }
{ELSE}      {return ELSE; }
{IF}          {return IF; }
{FI}      {return FI; }
{IN}      {return IN; }
{INHERITS}      {return INHERITS; }
{ISVOID}      {return ISVOID; }
{LET}      {return LET; }
{LOOP}      {return LOOP; }
{POOL}     { return POOL; }
{THEN}     { return THEN; }
{WHILE}    { return WHILE; }
{CASE}     { return CASE; }
{ESAC}     { return ESAC; }
{OF}       { return OF; }
{NEW}      { return NEW; }
{ISVOID}   { return ISVOID; }
{NOT}      { return NOT; }

[0-9]+ {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}

t(?i:RUE) {
  cool_yylval.boolean = 1;
  return BOOL_CONST;
}

f(?i:ALSE) {
  cool_yylval.boolean = 0;
  return BOOL_CONST;
}

[ \t\r\f\v]+ { }

{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

{OBJECTID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

 /*
  *  The multiple-character operators.
  */

"\n" {
    curr_lineno++;
}

"=>" { return DARROW; }

"<-" { return ASSIGN; }

"<=" { return LE; }

"+" { return (int)('+'); }

"-" { return (int)('-'); }

"*" { return (int)('*'); }

"/" { return (int)('/'); }

"<" { return (int)('<'); }

"=" { return (int)('='); }

"." { return (int)('.'); }

";" { return (int)(';'); }

"~" { return (int)('~'); }

"{" { return (int)('{'); }

"}" { return (int)('}'); }

"(" { return (int)('('); }

")" { return (int)(')'); }

":" { return (int)(':'); }

"@" { return (int)('@'); }

"," { return (int)(','); }

 /* error */

[^\n] {
    yylval.error_msg = yytext;
    return ERROR;
}

%%
