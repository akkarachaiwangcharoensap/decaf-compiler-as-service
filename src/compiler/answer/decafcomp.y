%{
#include <iostream>
#include <ostream>
#include <string>
#include <cstdlib>
#include "default-defs.h"

#include "classes/llvm/symbol_table.h"

int yylex(void);
int yyerror(char *); 

// print AST?
bool printAST = false;
bool printLLVM = true;

using namespace std;

#include "decafcomp.cc"

%}

%define parse.error verbose

%union {
    class decafAST *ast;
    class decafStmtList* slist;
    std::string *sval;
    std::list<class ParameterAST*> *paramlist;
}

%token T_PACKAGE
%token T_LCB
%token T_RCB

%token T_FUNC

%token T_LPAREN
%token T_RPAREN
%token T_VOID

%token T_AND
%token T_ASSIGN
%token T_BOOLTYPE
%token T_BREAK
%token <sval> T_CHARCONSTANT
%token T_COMMA
%token T_COMMENT
%token T_CONTINUE
%token T_DIV
%token T_DOT
%token T_ELSE
%token T_EQ
%token T_EXTERN
%token T_FALSE
%token T_FOR
%token T_GEQ
%token T_GT
%token T_IF
%token <sval> T_INTCONSTANT
%token T_INTTYPE
%token T_LEFTSHIFT
%token T_LEQ
%token T_LSB
%token T_LT
%token T_MINUS
%token T_MOD
%token T_MULT
%token T_NEQ
%token T_NOT
%token T_NULL
%token T_OR

%token T_PLUS
%token T_RETURN
%token T_RIGHTSHIFT
%token T_RSB
%token T_SEMICOLON
%token <sval> T_STRINGCONSTANT
%token T_STRINGTYPE
%token T_TRUE
%token T_VAR
%token T_WHILE
%token T_WHITESPACE

%token <sval> T_ID

%type <ast> field_decl field_list var_decl var_list

%type <ast> extern_list decafpackage

%type <ast> extern_decl extern_type_list extern_type

%type <slist> id_list

%type <slist> method_args
%type <ast> method_decl method_list method_type method_call method_arg

%type <ast> stmt_list stmt block

%type <ast> expr lvalue assign assign_list constant

%type <ast> decaf_type array_type

// Operators and Precedence
%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_LEQ T_GT T_GEQ 		// allow chaining
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%right T_NOT
%right T_UMinus

%type <paramlist> param_list

%%

start: program

program: extern_list decafpackage
{
    ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2); 
    if (printAST) {
        cout << getString(prog) << endl;
    }

    // Construct LLVM objects
    llvm::LLVMContext llvmContext;
    llvm::IRBuilder<> builder(llvmContext);
    auto module = std::make_unique<llvm::Module>("DecafProgram", llvmContext);
    CodegenContext ctx(llvmContext, builder, module.get());

    try {
        

        // do optimalization stuff - Sam

        // Push initial scope
        ctx.symbols.push_scope();

        prog->Codegen(ctx);

        // Optionally print generated IR
        if (printLLVM) {
            // module->print(llvm::outs(), nullptr);
            module->print(llvm::errs(), nullptr);

        }

        ctx.symbols.pop_scope();
    } 
    catch (std::runtime_error &e) {
        if (printLLVM) {
            // module->print(llvm::outs(), nullptr);
            module->print(llvm::errs(), nullptr);

        }
        cout << "semantic error: " << e.what() << endl;
        exit(EXIT_FAILURE);
    }

    delete prog;
}
;

extern_decl:
    T_EXTERN T_FUNC T_ID T_LPAREN extern_type_list T_RPAREN method_type T_SEMICOLON
    {
        $$ = new ExternAST(*$3, (decafStmtList *)$5, dynamic_cast<MethodType*>($7));
        delete $3;
    }
;

extern_list:
    /* extern_list can be empty */ { $$ = new decafStmtList(); }
    | extern_list extern_decl
    {
        ((decafStmtList *)$1)->push_back($2);
        $$ = $1;
    }
;

array_type:
    T_LSB T_INTCONSTANT T_RSB decaf_type
    {
        $$ = new ArrayType(dynamic_cast<DecafType*>($4), *$2);
        delete $2;
    }
;

decaf_type:
    T_INTTYPE      { $$ = new DecafType("IntType"); }
    | T_BOOLTYPE     { $$ = new DecafType("BoolType"); }

extern_type:
    T_STRINGTYPE     { $$ = new ExternType("StringType"); }
;

extern_type_list:
    /* extern_type_list can be empty */ { $$ = new decafStmtList(); }
    | extern_type
    {
        decafStmtList *list = new decafStmtList();
        list->push_back(dynamic_cast<ExternType*>($1));
        $$ = list;
    }
    | decaf_type
    {
        decafStmtList *list = new decafStmtList();
        list->push_back(new ExternType(dynamic_cast<DecafType*>($1)->str()));
        $$ = list;
    }
    | extern_type_list T_COMMA extern_type
    {
        ((decafStmtList *)$1)->push_back(dynamic_cast<ExternType*>($3));
        $$ = $1;
    }
    | extern_type_list T_COMMA decaf_type
    {
        ((decafStmtList *)$1)->push_back(dynamic_cast<DecafType*>($3));
        $$ = $1;
    }
;

// Global variables
// S/R conflict since id_list can be just T_ID (first and second line the same)
field_decl:
    T_VAR id_list decaf_type T_SEMICOLON
    {
        $$ = new FieldDeclAST(dynamic_cast<DecafType*>($3), $2, new ScalarType());  // DecafType + multiple vars
    }
    // Global variable assignment
    | T_VAR id_list decaf_type T_ASSIGN constant T_SEMICOLON
    {
        // Check that exactly one identifier is provided for assignment
        if ($2->size() != 1) {
            yyerror("Variable assignment requires exactly one identifier");
            YYERROR;
        }
        $$ = new FieldAssignAST(dynamic_cast<DecafType*>($3), $2, dynamic_cast<ConstantAST*>($5));
    }
    // Array
    | T_VAR id_list array_type T_SEMICOLON
    {
        $$ = new ArrayDeclAST($2, $3);
    }
;

id_list: 
    T_ID {
        $$ = new decafStmtList();
        $$->push_back(new SimpleIDAST(*$1));
        delete $1;
    }
    | id_list T_COMMA T_ID {
        $1->push_back(new SimpleIDAST(*$3));
        delete $3;
        $$ = $1;
    }
;

// Local variables
var_decl: 
    T_VAR id_list decaf_type T_SEMICOLON 
    { 
        $$ = new VarDeclAST(dynamic_cast<DecafType*>($3), $2); 
    }
;

field_list:
    /* field_list can be empty */ { $$ = new decafStmtList(); }
    | field_list field_decl 
    { 
        ((decafStmtList *)$1)->push_back($2); 
        $$ = $1; 
    }
;

var_list:
    /* var_list can be empty */ { $$ = new decafStmtList(); }
    | var_list var_decl 
    { 
        ((decafStmtList *)$1)->push_back($2); 
        $$ = $1; 
    }
;

param_list:
    /* param_list can be empty */ { $$ = new std::list<ParameterAST*>(); }

    | T_ID decaf_type {
        $$ = new std::list<ParameterAST*>();
        $$->push_back(new ParameterAST(*$1, dynamic_cast<DecafType*>($2)));
        delete $1;
    }

    | param_list T_COMMA T_ID decaf_type {
        $$ = $1;
        $$->push_back(new ParameterAST(*$3, dynamic_cast<DecafType*>($4)));
        delete $3;
    }
;

method_type:
    T_VOID           { $$ = new MethodType("VoidType"); }
    | T_INTTYPE      { $$ = new MethodType("IntType"); }
    | T_BOOLTYPE     { $$ = new MethodType("BoolType"); }
;

method_list:
    /* method_list can be empty */
    { $$ = new decafStmtList(); }
    | method_list method_decl
    {
        ((decafStmtList *)$1)->push_back($2);
        $$ = $1;
    }
;

stmt_list:
    /* stmt_list stmt_list can be empty */ { $$ = new decafStmtList(); }
    | stmt_list stmt
    { ((decafStmtList *)$1)->push_back($2); $$ = $1; }
;

block:
    T_LCB var_list stmt_list T_RCB
    {
        $$ = new BlockAST((decafStmtList *)$2, (decafStmtList *)$3);
    }
;

stmt:
    block { $$ = $1; }
    | assign T_SEMICOLON            { $$ = $1; }
    | method_call T_SEMICOLON       { $$ = $1; }
    | T_IF T_LPAREN expr T_RPAREN block
      { $$ = new IfAST($3, $5, nullptr); }
    | T_IF T_LPAREN expr T_RPAREN block T_ELSE block
      { $$ = new IfAST($3, $5, $7); }
    | T_WHILE T_LPAREN expr T_RPAREN block
      { $$ = new WhileAST($3, $5); }
    | T_FOR T_LPAREN assign_list T_SEMICOLON expr T_SEMICOLON assign_list T_RPAREN block
      { $$ = new ForAST($3, $5, $7, $9); }
    | T_RETURN T_SEMICOLON
      { $$ = new ReturnAST(nullptr); }
    | T_RETURN T_LPAREN T_RPAREN T_SEMICOLON
      { $$ = new ReturnAST(nullptr); }
    | T_RETURN T_LPAREN expr T_RPAREN T_SEMICOLON
      { $$ = new ReturnAST($3); }
    | T_BREAK T_SEMICOLON
      { $$ = new BreakAST(); }
    | T_CONTINUE T_SEMICOLON
      { $$ = new ContinueAST(); }

method_decl:
    T_FUNC T_ID T_LPAREN param_list T_RPAREN method_type block
    {
        MethodBlockAST* methodBlock = new MethodBlockAST(dynamic_cast<BlockAST*>($7)->getField(),
                                                         dynamic_cast<BlockAST*>($7)->getStmt());

        $$ = new MethodAST(*$2, *$4, dynamic_cast<MethodType*>($6), methodBlock);
        delete $2;
        delete $4;
    }
;

assign:
    lvalue T_ASSIGN expr
    { 
        SimpleIDAST* id = dynamic_cast<SimpleIDAST*>($1);
        if (id) {
            $$ = new AssignAST(new VariableExprAST(id->str()), $3);
            delete id;
        } else {
            ArrayIDAST* array = dynamic_cast<ArrayIDAST*>($1);
            if (!array) {
                yyerror("Invalid lvalue in assignment");
                YYABORT;
            }

            $$ = new ArrayAssignAST(array->getID(), array->getIndex(), $3);
            delete array;
        }
    }
;

assign_list:
    assign {
        decafStmtList* list = new decafStmtList();
        list->push_back($1);
        $$ = list;
    }
    | assign_list T_COMMA assign {
        decafStmtList* list = dynamic_cast<decafStmtList*>($1);
        list->push_back($3);
        $$ = list;
    }
;

lvalue:
    T_ID                          { $$ = new SimpleIDAST(*$1); delete $1; }
    // Array indexing
    | T_ID T_LSB T_INTCONSTANT T_RSB       { $$ = new ArrayIDAST(*$1, std::stoi(*$3)); delete $1; }
;

method_call:
    T_ID T_LPAREN method_args T_RPAREN
    { $$ = new MethodCallAST(*$1, (decafStmtList *)$3); delete $1; }
;

method_arg:
    expr { $$ = $1; }
    | T_STRINGCONSTANT {
        $$ = new ConstantAST("StringConstant", *$1);
        delete $1;
    }
;

method_args:
    /* method_args can be empty */ { $$ = new decafStmtList(); }
    | method_arg {
        decafStmtList *list = new decafStmtList();
        list->push_back($1); $$ = list;
    }
    | method_args T_COMMA method_arg
    { ((decafStmtList *)$1)->push_back($3); $$ = $1; }
;

constant:
    T_CHARCONSTANT      { $$ = new ConstantAST("NumberExpr", *$1); delete $1; }
    | T_INTCONSTANT     { $$ = new ConstantAST("NumberExpr", *$1); delete $1; }
    | T_TRUE            { $$ = new ConstantAST("BoolExpr", "True"); }
    | T_FALSE           { $$ = new ConstantAST("BoolExpr", "False"); }
;

expr:
      T_ID                        { $$ = new VariableExprAST(*$1); delete $1; }
    | constant                    { $$ = $1; }
    | method_call                 { $$ = $1; }
    | expr T_PLUS expr            { $$ = new BinaryOpAST("Plus", $1, $3); }
    | expr T_MINUS expr           { $$ = new BinaryOpAST("Minus", $1, $3); }
    | expr T_MULT expr            { $$ = new BinaryOpAST("Mult", $1, $3); }
    | expr T_DIV expr             { $$ = new BinaryOpAST("Div", $1, $3); }
    | expr T_MOD expr             { $$ = new BinaryOpAST("Mod", $1, $3); }
    | expr T_AND expr             { $$ = new BinaryOpAST("And", $1, $3); }
    | expr T_OR expr              { $$ = new BinaryOpAST("Or", $1, $3); }
    | expr T_EQ expr              { $$ = new BinaryOpAST("Eq", $1, $3); }
    | expr T_NEQ expr             { $$ = new BinaryOpAST("Neq", $1, $3); }
    | expr T_LT expr              { $$ = new BinaryOpAST("Lt", $1, $3); }
    | expr T_LEQ expr             { $$ = new BinaryOpAST("Leq", $1, $3); }
    | expr T_GT expr              { $$ = new BinaryOpAST("Gt", $1, $3); }
    | expr T_GEQ expr             { $$ = new BinaryOpAST("Geq", $1, $3); }
    | T_NOT expr                  { $$ = new UnaryOpAST("Not", $2); }
    | T_MINUS expr %prec T_UMinus { $$ = new UnaryOpAST("UnaryMinus", $2); }
    | expr T_LEFTSHIFT expr       { $$ = new BinaryOpAST("Leftshift", $1, $3); }
    | expr T_RIGHTSHIFT expr      { $$ = new BinaryOpAST("Rightshift", $1, $3); }
    | T_LPAREN expr T_RPAREN      { $$ = $2; }
    | T_ID T_LSB T_INTCONSTANT T_RSB       { $$ = new ArrayIDAST(*$1, std::stoi(*$3)); delete $1; }

decafpackage: T_PACKAGE T_ID T_LCB field_list method_list T_RCB
    { 
        $$ = new PackageAST(*$2, (decafStmtList *)$4, (decafStmtList *)$5); 
        delete $2; 
    }
;

%%

int main() {
    try {
        // Just parse, all codegen is done in the grammar rule now
        return yyparse();
    }
    catch (std::runtime_error &e) {
        std::cerr << "Codegen error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
}
