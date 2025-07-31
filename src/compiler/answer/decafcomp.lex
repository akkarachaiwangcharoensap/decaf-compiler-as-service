%{
#include "decafcomp-defs.h"
#include "decafcomp.tab.h"
#include <cstring>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;

extern YYLTYPE yylloc;

// int lineno = 1;
// int tokenpos = 1;
int lineno = 1;
int column = 1;

// #define UPDATE_TOKENPOS yytext ? tokenpos += yyleng : tokenpos

// first column happens before codegen.
// yylloc.first_column: -1 to offset the first character, happens before codegen.
// yylloc.last_column: happens after codegen.
#define YY_USER_ACTION \
    yylloc.first_line = yylloc.last_line = lineno; \
    yylloc.first_column = column - 1; \
    yylloc.last_column = column + yyleng - 1; \
    // column += yyleng;
%}

/* regexp definitions */

char_for_strings [\x07-\x09]|[\x0B-\x0D]|[\x20-\x21]|[\x23-\x5B]|[\x5D-\x7E]
char_lit_chars [\x07-\x0D]|[\x20-\x26]|[\x28-\x5B]|[\x5D-\x7E]
char_no_nl [\x07-\x09]|[\x0B-\x0D]|[\x20-\x7E]

letter [a-z]|[A-Z]|_
decimal_digit [0-9]
hex_digit [0-9]|[A-F]|[a-f]
digit [0-9]

decimal_lit {decimal_digit}+
hex_lit 0(x|X){hex_digit}+

escaped_char (\\[nrtvfab\\'\"])

%%
  /*
    Pattern definitions for all tokens 
  */

  /* Keywords */
bool                                        { return T_BOOLTYPE; }
break                                       { return T_BREAK; }
continue                                    { return T_CONTINUE; }
else                                        { return T_ELSE; }
extern                                      { return T_EXTERN; }
false                                       { return T_FALSE; }
for                                         { return T_FOR; }
func                                        { return T_FUNC; }
if                                          { return T_IF; }
int                                         { return T_INTTYPE; }
null                                        { return T_NULL; }
package                                     { return T_PACKAGE; }
return                                      { return T_RETURN; }
string                                      { return T_STRINGTYPE; }
true                                        { return T_TRUE; }
var                                         { return T_VAR; }
void                                        { return T_VOID; }
while                                       { return T_WHILE; }

  /* Operators and Delimiters */
\{                                          { return T_LCB; }  
\}                                          { return T_RCB; }  
\[                                          { return T_LSB; }
\]                                          { return T_RSB; }
,                                           { return T_COMMA; }
;                                           { return T_SEMICOLON; }
\(                                          { return T_LPAREN; }
\)                                          { return T_RPAREN; }
=                                           { return T_ASSIGN; } 
\-                                          { return T_MINUS; }
!                                           { return T_NOT; }
\+                                          { return T_PLUS; }
\*                                          { return T_MULT; }
\/                                          { return T_DIV; }
\<\<                                        { return T_LEFTSHIFT; }
\>\>                                        { return T_RIGHTSHIFT; }
\<                                          { return T_LT; }
\>                                          { return T_GT; }  
%                                           { return T_MOD; }
\<=                                         { return T_LEQ; }
\>=                                         { return T_GEQ; }
==                                          { return T_EQ; }
!=                                          { return T_NEQ; }
&&                                          { return T_AND; }
\|\|                                        { return T_OR; }
\.                                          { return T_DOT; }

  /* Integer, Character, String literals */
'{char_lit_chars}'|'{escaped_char}'         { 
    char c;
    if (yytext[1] != '\\') {
        c = yytext[1];
    } else {
        switch (yytext[2]) {
            case 'n': c = '\n'; break;
            case 't': c = '\t'; break;
            case 'r': c = '\r'; break;
            case 'v': c = '\v'; break;
            case 'f': c = '\f'; break;
            case 'a': c = '\a'; break;
            case 'b': c = '\b'; break;
            case '\\': c = '\\'; break;
            case '\'': c = '\''; break;
            case '\"': c = '\"'; break;
            default:
                cerr << "Unknown escape sequence \\" << yytext[2]
                     << " at line " << lineno << ", column " << column << endl;
                c = '?'; // fallback to placeholder
        }
    }

    yylval.sval = new string(to_string((int)c)); 
    return T_CHARCONSTANT; 
}
\"({char_for_strings}|{escaped_char})*\"    { yylval.sval = new string(yytext); return T_STRINGCONSTANT; }
{decimal_lit}|{hex_lit}                     { yylval.sval = new string(yytext); return T_INTCONSTANT; }

  /* Symbols */
{letter}({letter}|{digit})*             { 
  yylval.sval = new string(yytext); 
  return T_ID; } /* note that identifier pattern must be after all keywords */

[\n]                                    { lineno++; column = 1; /* newline */ }
  
  /* '\r', '\t', Vertical tab, Form feed, Space */
(\x0D|\x09|\x0B|\x0C|\x20)+             { column += yyleng; }

  /* Comments */
\/\/{char_no_nl}*\n                     { lineno++; column = 1; }
. {
    cerr << "Error at line " << lineno << ", column " << column
         << ": unexpected character '" << yytext[0] << "'" << endl;
    column += 1;
    return -1;
}

%%
int yyerror(const char *s) {
  cerr << "Syntax error at line " << lineno
       << ", char " << column
       << " near \"" << yytext << "\": " << s << endl;
  return 1;
}
