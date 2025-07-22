#ifndef EXPRESSION_COMP_H
#define EXPRESSION_COMP_H

#include <string>
#include "../decafcomp.cc"

/// BinaryOpAST - the binary operator
class BinaryOpAST : public decafAST {
    string Op;
    decafAST* LHS;
    decafAST* RHS;
public:
    BinaryOpAST(string op, decafAST* lhs, decafAST* rhs) : Op(op), LHS(lhs), RHS(rhs) {}
    ~BinaryOpAST() { delete LHS; delete RHS; }
    string str() override {
        return "BinaryExpr(" + Op + "," + getString(LHS) + "," + getString(RHS) + ")";
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        llvm::Value* L = LHS->Codegen(ctx);
        llvm::Value* R = RHS->Codegen(ctx);
        if (!L || !R) return nullptr;

        if (Op == "Plus") {
            return ctx.builder.CreateAdd(L, R, "addtmp");
        } else if (Op == "Minus") {
            return ctx.builder.CreateSub(L, R, "subtmp");
        } else if (Op == "Mult") {
            return ctx.builder.CreateMul(L, R, "multmp");
        } else if (Op == "Div") {
            // signed division
            return ctx.builder.CreateSDiv(L, R, "divtmp");
        } else if (Op == "Mod") {
            // signed modulo
            return ctx.builder.CreateSRem(L, R, "modtmp");
        } else if (Op == "Eq") {
            return ctx.builder.CreateICmpEQ(L, R, "eqtmp");
        } else if (Op == "Neq") {
            return ctx.builder.CreateICmpNE(L, R, "netmp");
        } else if (Op == "Lt") {
            return ctx.builder.CreateICmpSLT(L, R, "lttmp");
        } else if (Op == "Leq") {
            return ctx.builder.CreateICmpSLE(L, R, "letmp");
        } else if (Op == "Gt") {
            return ctx.builder.CreateICmpSGT(L, R, "gttmp");
        } else if (Op == "Geq") {
            return ctx.builder.CreateICmpSGE(L, R, "getmp");
        } else if (Op == "And") {
            return ctx.builder.CreateAnd(L, R, "andtmp");
        } else if (Op == "Or") {
            return ctx.builder.CreateOr(L, R, "ortmp");
        } else if (Op == "Leftshift") {
			return ctx.builder.CreateShl(L, R, "shltmp");
		} else if (Op == "Rightshift") {
			return ctx.builder.CreateLShr(L, R, "lshrtmp");
		} else {
            throw std::runtime_error("Unknown binary operator: " + Op);
            return nullptr;
        }
    }
};

class UnaryOpAST : public decafAST {
    string Op;
    decafAST* Expr;
public:
    UnaryOpAST(string op, decafAST* expr) : Op(op), Expr(expr) {}
    ~UnaryOpAST() { delete Expr; }
    string str() override {
        return "UnaryExpr(" + Op + "," + getString(Expr) + ")";
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        llvm::Value* E = Expr->Codegen(ctx);
        if (!E) return nullptr;

        if (Op == "UnaryMinus") {
            return ctx.builder.CreateNeg(E, "negtmp");
        } else if (Op == "Not") {
            return ctx.builder.CreateNot(E, "nottmp");
        } else {
            std::cerr << "Unknown unary operator: " << Op << std::endl;
            return nullptr;
        }
    }
};

// Variable references in expressions
class VariableExprAST : public decafAST {
	string Name;

public:
	VariableExprAST(const string& name) : Name(name) {}
	~VariableExprAST() {}

    string getName() const { return Name; }

	string str() override {
		return "VariableExpr(" + Name + ")";
	}

    llvm::Value *Codegen(CodegenContext& ctx) override {
        Descriptor* desc = ctx.symbols.lookup(Name);
        if (!desc) {
            throw std::runtime_error("Undeclared variable: " + Name);
        }

        if (!desc->getValue()) {
            throw std::runtime_error("Descriptor has null value: " + Name);
        }

        if (!desc->getType()) {
            throw std::runtime_error("Descriptor has null type: " + Name);
        }

        return ctx.builder.CreateLoad(desc->getType(), desc->getValue(), Name + "_val");
    }
};

class AssignAST : public decafAST {
    decafAST* LHS;
    decafAST* RHS;
public:
    AssignAST(decafAST* lhs, decafAST* rhs) : LHS(lhs), RHS(rhs) {}
    ~AssignAST() { delete LHS; delete RHS; }
    string str() override {
        return "AssignVar(" + getString(LHS) + "," + getString(RHS) + ")";
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        llvm::Value* destPtr = nullptr;

        // Handle scalar variable assignment
        if (auto* varExpr = dynamic_cast<VariableExprAST*>(LHS)) {
            Descriptor* desc = ctx.symbols.lookup(varExpr->getName());
            if (!desc) {
                throw std::runtime_error("Undeclared variable: " + varExpr->getName());
            }
            destPtr = desc->getValue();
        }
        else {
            throw std::runtime_error("LHS of assignment must be a variable or array access: " + LHS->str());
        }

        llvm::Value* rhsVal = RHS->Codegen(ctx);
        if (!rhsVal) {
            throw std::runtime_error("Invalid RHS expression in assignment");
        }

        return ctx.builder.CreateStore(rhsVal, destPtr);
    }
};

class ArrayAssignAST : public decafAST {
    std::string ID;
    int Index;
    decafAST* Value;

public:
    ArrayAssignAST(std::string id, int index, decafAST* value)
        : ID(id), Index(index), Value(value) {}

    ~ArrayAssignAST() { delete Value; }

    std::string str() override {
        return "AssignArrayLoc(" + ID + ",NumberExpr(" + std::to_string(Index) + ")," + getString(Value) + ")";
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        Descriptor* desc = ctx.symbols.lookup(ID);
        if (!desc) throw std::runtime_error("Undeclared array: " + ID);

        llvm::Value* arrayPtr = desc->getValue();

        llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(desc->getType());
        if (!arrayType)
            throw std::runtime_error("Expected array type for variable: " + ID);

        llvm::Value* zero = llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0);
        llvm::Value* indexVal = llvm::ConstantInt::get(ctx.builder.getInt32Ty(), Index);
        llvm::Value* elementPtr = ctx.builder.CreateGEP(arrayType, arrayPtr, {zero, indexVal}, ID + "_elt");

        llvm::Value* rhsVal = Value->Codegen(ctx);
        llvm::Type* elemType = arrayType->getElementType();

        return ctx.builder.CreateStore(rhsVal, elementPtr);
    }
};

#endif // EXPERSSION_COMP_H