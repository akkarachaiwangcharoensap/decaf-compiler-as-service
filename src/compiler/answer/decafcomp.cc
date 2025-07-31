#ifndef DECAF_COMP_H
#define DECAF_COMP_H

#include "decafcomp-defs.h"
#include <list>
#include <ostream>
#include <iostream>
#include <sstream>

#ifndef YYTOKENTYPE
#include "decafcomp.tab.h"
#endif

#include "classes/llvm/codegen_context.h"

using namespace std;

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
protected:
	int line;
	int col;
	
	void semantic_error(const std::string& message) {
		std::ostringstream oss;
		oss << "at line " << std::to_string(this->getLine()) << ", column " << std::to_string(this->getCol()) << ": " << message;
		throw std::runtime_error(oss.str());
	}

public:
	virtual ~decafAST() {}

	virtual string str() { return string(""); }
	virtual llvm::Value *Codegen(CodegenContext& ctx) = 0;

	void setLine(int l) { 
		line = l;
	}

    void setCol(int c) { 
		col = c;
	}

  	virtual int getLine() const { return line; }
    virtual int getCol() const { return col; }
};

string getString(decafAST *d) {
	if (d != NULL) {
		return d->str();
	} else {
		return string("None");
	}
}

inline std::string llvmTypeToString(llvm::Type *T) {
    std::string s;
    llvm::raw_string_ostream os(s);
    T->print(os);
    return os.str();
}

inline llvm::Value* coerceToInt32(llvm::IRBuilder<>& builder, llvm::Value* val) {
    if (val->getType()->isIntegerTy(1)) {
        return builder.CreateZExt(val, llvm::Type::getInt32Ty(builder.getContext()), "boolToInt");
    }
    return val;
}

template <class T>
string commaList(list<T> vec) {
    string s("");
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) { 
        s = s + (s.empty() ? string("") : string(",")) + (*i)->str(); 
    }   
    if (s.empty()) {
        s = string("None");
    }   
    return s;
}

template <class T>
llvm::Value *listCodegen(list<T> vec, CodegenContext& ctx) {
	llvm::Value *val = nullptr;
	for (auto &item : vec) {
		llvm::Value *j = item->Codegen(ctx);
		if (j != nullptr) val = j;
	}
	return val;
}

/// DecafType - decaf type (int | bool)
class DecafType : public decafAST {
    string Type;
public:
    DecafType(const string& type) : Type(type) {}
    ~DecafType() {}
    string str() override {
		return "" + Type + "";
    }
	llvm::Value *Codegen(CodegenContext& ctx) override {
		// For now, if it doesn't generate code, return nullptr
		return nullptr;
	}
};

/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
	list<decafAST *> stmts;
public:
	decafStmtList() {}
	~decafStmtList() {
		for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) { 
			delete *i;
		}
	}
	int size() { return stmts.size(); }
	void push_front(decafAST *e) { stmts.push_front(e); }
	void push_back(decafAST *e) { stmts.push_back(e); }
	string str() { return commaList<class decafAST *>(stmts); }
	std::list<decafAST *>::iterator begin() { return stmts.begin(); }
	std::list<decafAST *>::iterator end() { return stmts.end(); }
	llvm::Value *Codegen(CodegenContext& ctx) {
		return listCodegen<decafAST*>(stmts, ctx);
	}
};

class BlockAST : public decafAST {
    decafStmtList* FieldList;
    decafStmtList* StmtList;
public:
    BlockAST(decafStmtList* fields, decafStmtList* stmts) : FieldList(fields), StmtList(stmts) {}
    ~BlockAST() {
        delete FieldList;
        delete StmtList;
    }

	decafStmtList* getField() {return FieldList;}
	decafStmtList* getStmt() {return StmtList;}

    string str() override {
        return "Block(" + getString(FieldList) + "," + getString(StmtList) + ")";
    }

	llvm::Value* Codegen(CodegenContext& ctx) override {
		// Start a new scope
		ctx.symbols.push_scope();

		llvm::Value* val = nullptr;
		if (FieldList) val = FieldList->Codegen(ctx);
		if (StmtList) val = StmtList->Codegen(ctx);

		// End the new scope
		ctx.symbols.pop_scope();
		return val;
	}
};

// Include other related AST nodes
#include "classes/field_comp.h"
#include "classes/method_comp.h"
#include "classes/extern_comp.h"
#include "classes/expression_comp.h"
#include "classes/statement_comp.h"

#endif // DECAF_COMP_H
