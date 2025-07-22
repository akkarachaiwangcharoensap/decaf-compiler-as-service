%{
#include "decafcomp-defs.h"
#include "decafcomp.tab.h"
#include <cstring>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;

int lineno = 1;
int tokenpos = 1;

#define UPDATE_TOKENPOS yytext ? tokenpos += yyleng : tokenpos

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
bool                                        { UPDATE_TOKENPOS; return T_BOOLTYPE; }
break                                       { UPDATE_TOKENPOS; return T_BREAK; }
continue                                    { UPDATE_TOKENPOS; return T_CONTINUE; }
else                                        { UPDATE_TOKENPOS; return T_ELSE; }
extern                                      { UPDATE_TOKENPOS; return T_EXTERN; }
false                                       { UPDATE_TOKENPOS; return T_FALSE; }
for                                         { UPDATE_TOKENPOS; return T_FOR; }
func                                        { UPDATE_TOKENPOS; return T_FUNC; }
if                                          { UPDATE_TOKENPOS; return T_IF; }
int                                         { UPDATE_TOKENPOS; return T_INTTYPE; }
null                                        { UPDATE_TOKENPOS; return T_NULL; }
package                                     { UPDATE_TOKENPOS; return T_PACKAGE; }
return                                      { UPDATE_TOKENPOS; return T_RETURN; }
string                                      { UPDATE_TOKENPOS; return T_STRINGTYPE; }
true                                        { UPDATE_TOKENPOS; return T_TRUE; }
var                                         { UPDATE_TOKENPOS; return T_VAR; }
void                                        { UPDATE_TOKENPOS; return T_VOID; }
while                                       { UPDATE_TOKENPOS; return T_WHILE; }

  /* Operators and Delimiters */
\{                                          { UPDATE_TOKENPOS; return T_LCB; }  
\}                                          { UPDATE_TOKENPOS; return T_RCB; }  
\[                                          { UPDATE_TOKENPOS; return T_LSB; }
\]                                          { UPDATE_TOKENPOS; return T_RSB; }
,                                           { UPDATE_TOKENPOS; return T_COMMA; }
;                                           { UPDATE_TOKENPOS; return T_SEMICOLON; }
\(                                          { UPDATE_TOKENPOS; return T_LPAREN; }
\)                                          { UPDATE_TOKENPOS; return T_RPAREN; }
=                                           { UPDATE_TOKENPOS; return T_ASSIGN; } 
\-                                          { UPDATE_TOKENPOS; return T_MINUS; }
!                                           { UPDATE_TOKENPOS; return T_NOT; }
\+                                          { UPDATE_TOKENPOS; return T_PLUS; }
\*                                          { UPDATE_TOKENPOS; return T_MULT; }
\/                                          { UPDATE_TOKENPOS; return T_DIV; }
\<\<                                        { UPDATE_TOKENPOS; return T_LEFTSHIFT; }
\>\>                                        { UPDATE_TOKENPOS; return T_RIGHTSHIFT; }
\<                                          { UPDATE_TOKENPOS; return T_LT; }
\>                                          { UPDATE_TOKENPOS; return T_GT; }  
%                                           { UPDATE_TOKENPOS; return T_MOD; }
\<=                                         { UPDATE_TOKENPOS; return T_LEQ; }
\>=                                         { UPDATE_TOKENPOS; return T_GEQ; }
==                                          { UPDATE_TOKENPOS; return T_EQ; }
!=                                          { UPDATE_TOKENPOS; return T_NEQ; }
&&                                          { UPDATE_TOKENPOS; return T_AND; }
\|\|                                        { UPDATE_TOKENPOS; return T_OR; }
\.                                          { UPDATE_TOKENPOS; return T_DOT; }

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
                     << " at line " << lineno << ", column " << tokenpos << endl;
                c = '?'; // fallback to placeholder
        }
    }

    yylval.sval = new string(to_string((int)c)); 
    UPDATE_TOKENPOS; 
    return T_CHARCONSTANT; 
}
\"({char_for_strings}|{escaped_char})*\"    { yylval.sval = new string(yytext); UPDATE_TOKENPOS; return T_STRINGCONSTANT; }
{decimal_lit}|{hex_lit}                     { yylval.sval = new string(yytext); UPDATE_TOKENPOS; return T_INTCONSTANT; }

  /* Symbols */
{letter}({letter}|{digit})*             { 
  yylval.sval = new string(yytext); 
  UPDATE_TOKENPOS; 
  return T_ID; } /* note that identifier pattern must be after all keywords */

[\n]                                    { lineno++; tokenpos = 1; /* newline */ }
(\x0D|\x09|\x0B|\x0C|\x20)+             { UPDATE_TOKENPOS; }

  /* Comments */
\/\/{char_no_nl}*\n                     { lineno++; tokenpos = 1; }
. {
    cerr << "Error at line " << lineno << ", column " << tokenpos
         << ": unexpected character '" << yytext[0] << "'" << endl;
    tokenpos += 1;
    return -1;
}

%%
int yyerror(const char *s) {
  cerr << "Syntax error at line " << lineno
       << ", char " << tokenpos
       << " near \"" << yytext << "\": " << s << endl;
  return 1;
}
