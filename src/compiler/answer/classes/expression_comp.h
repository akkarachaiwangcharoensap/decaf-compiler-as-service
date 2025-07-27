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

	// Helpers for short-circuiting boolean operations
	llvm::Value* ShortCircuitAnd(CodegenContext& ctx) {
		// Need to evaluate left side either way
		llvm::Value* leftVal = LHS->Codegen(ctx);
		if (!leftVal) return nullptr;

		// Convert to boolean (0->false, else true)
		leftVal = ctx.builder.CreateICmpNE(
			leftVal,
			llvm::ConstantInt::get(leftVal->getType(), 0),
			"leftbool"
		);

		// Get context + pointer to current block (for PHI function)
		llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();
		llvm::BasicBlock* leftBB = ctx.builder.GetInsertBlock();

		// Basic blocks for flow control
		llvm::BasicBlock* rightBB = llvm::BasicBlock::Create(ctx.llvmContext, "and_right", function);
		llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx.llvmContext, "and_merge", function);

		// Left is false => skip right evaluation
		ctx.builder.CreateCondBr(leftVal, rightBB, mergeBB);

		// Else evaluate right side
		ctx.builder.SetInsertPoint(rightBB);
		llvm::Value* rightVal = RHS->Codegen(ctx);
		if (!rightVal) return nullptr;

		rightVal = ctx.builder.CreateICmpNE(
			rightVal, 
			llvm::ConstantInt::get(rightVal->getType(), 0),
			"rightbool"
		);

		ctx.builder.CreateBr(mergeBB);
		rightBB = ctx.builder.GetInsertBlock(); //update reference after build

		// Merge results using PHI function
		ctx.builder.SetInsertPoint(mergeBB);
		llvm::PHINode* result = ctx.builder.CreatePHI(ctx.builder.getInt1Ty(), 2, "andresult");
		result->addIncoming(llvm::ConstantInt::getFalse(ctx.llvmContext), leftBB);
		result->addIncoming(rightVal, rightBB);

		return result;
	}

	llvm::Value* ShortCircuitOr(CodegenContext& ctx) {
		llvm::Value* leftVal = LHS->Codegen(ctx);
		if (!leftVal) return nullptr;

		leftVal = ctx.builder.CreateICmpNE(
			leftVal,
			llvm::ConstantInt::get(leftVal->getType(), 0),
			"leftbool"
		);

		llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();
		llvm::BasicBlock* leftBB = ctx.builder.GetInsertBlock();

		llvm::BasicBlock* rightBB = llvm::BasicBlock::Create(ctx.llvmContext, "or_right", function);
		llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx.llvmContext, "or_merge", function);
		
		// Left is true => skip right evaluation and just merge
		ctx.builder.CreateCondBr(leftVal, mergeBB, rightBB);

		// Else evaluate(build) right side
		ctx.builder.SetInsertPoint(rightBB);
		llvm::Value* rightVal = RHS->Codegen(ctx);
		if (!rightVal) return nullptr;

		rightVal = ctx.builder.CreateICmpNE(
			rightVal,
			llvm::ConstantInt::get(rightVal->getType(), 0),
			"rightbool"
		);

		// Merge block
		ctx.builder.CreateBr(mergeBB);
		rightBB = ctx.builder.GetInsertBlock();

		ctx.builder.SetInsertPoint(mergeBB);
		llvm::PHINode* result = ctx.builder.CreatePHI(ctx.builder.getInt1Ty(), 2, "orresult");
		result->addIncoming(llvm::ConstantInt::getTrue(ctx.llvmContext), leftBB);
		result->addIncoming(rightVal, rightBB);

		return result;
	}

	llvm::Value *Codegen(CodegenContext& ctx) override {
		// Check for short-circuit first BEFORE evaluating L and R
		if (Op == "And") {
			return ShortCircuitAnd(ctx);
		} else if (Op == "Or") {
			return ShortCircuitOr(ctx);
		}

        llvm::Value* L = LHS->Codegen(ctx);
        llvm::Value* R = RHS->Codegen(ctx);
        if (!L || !R) return nullptr;

        llvm::Type *L_type = L->getType();
        llvm::Type *R_type = R->getType();

        if (Op == "Plus") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateAdd(L, R, "addtmp");
            else
                throw std::runtime_error("Addition must be ingeters in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Minus") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateSub(L, R, "subtmp");
            else
                throw std::runtime_error("Subtraction must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Mult") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateMul(L, R, "multmp");
            else
                throw std::runtime_error("Multiplication must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Div") {
            // signed division
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateSDiv(L, R, "divtmp");
            else
                throw std::runtime_error("Division must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Mod") {
            // signed modulo
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateSRem(L, R, "modtmp");
            else
                throw std::runtime_error("Modulo must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Eq") {
            if (L_type != R_type) 
                throw std::runtime_error("Types do not match in equality check: " + getString(LHS) + " vs " + getString(RHS));
            return ctx.builder.CreateICmpEQ(L, R, "eqtmp");
        } else if (Op == "Neq") {
            if (L_type != R_type) 
                throw std::runtime_error("Types do not match in inequality check: " + getString(LHS) + " vs " + getString(RHS));
            return ctx.builder.CreateICmpNE(L, R, "netmp");
        } else if (Op == "Lt") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateICmpSLT(L, R, "lttmp");
            else
                throw std::runtime_error("Less than must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Leq") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateICmpSLE(L, R, "letmp");
            else
                throw std::runtime_error("Less than or equal must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Gt") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateICmpSGT(L, R, "gttmp");
            else
                throw std::runtime_error("Greater than must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Geq") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateICmpSGE(L, R, "getmp");
            else
                throw std::runtime_error("Greater than or equal must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
        } else if (Op == "Leftshift") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateShl(L, R, "shltmp");
            else
                throw std::runtime_error("Left shift must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
		} else if (Op == "Rightshift") {
            if ((L_type->isIntegerTy(32) && R_type->isIntegerTy(32)))
                return ctx.builder.CreateLShr(L, R, "rshrtmp");
            else
                throw std::runtime_error("Right shift must be integers in BinaryOpAST LHS: " + llvmTypeToString(L_type) + " RHS: " + llvmTypeToString(R_type));
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
        llvm::Type* E_type = E->getType();
        if (!E) return nullptr;

        if (Op == "UnaryMinus") {
            if (!E_type->isIntegerTy(32))
                throw std::runtime_error("Unary minus must be applied to integers in UnaryOpAST");
            return ctx.builder.CreateNeg(E, "negtmp");
        } else if (Op == "Not") {
            if (!E_type->isIntegerTy(1))
                throw std::runtime_error("Logical NOT must be applied to booleans in UnaryOpAST");
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
        llvm::Type* dest_type = nullptr;
        // need to throw error if LHS is a array variable - Sam

        // Handle scalar variable assignment
        if (auto* varExpr = dynamic_cast<VariableExprAST*>(LHS)) {
            Descriptor* desc = ctx.symbols.lookup(varExpr->getName());
            if (!desc) {
                throw std::runtime_error("Undeclared variable: " + varExpr->getName());
            }
            destPtr = desc->getValue();
            dest_type = desc->getType();
        }
        else {
            throw std::runtime_error("LHS of assignment must be a variable or array access: " + LHS->str());
        }

        llvm::Value* rhsVal = RHS->Codegen(ctx);


        if (!rhsVal) {
            throw std::runtime_error("Invalid RHS expression in assignment");
        }

        // need to make sure LHS and RHS types match - Sam
        
        llvm::Type* rhs_type = rhsVal->getType();
        if (dest_type != rhs_type) {
            throw std::runtime_error("Type mismatch in assignment: " + llvmTypeToString(dest_type) + " vs " + llvmTypeToString(rhs_type));
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