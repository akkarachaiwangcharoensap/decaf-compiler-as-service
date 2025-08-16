#ifndef METHOD_COMP_H
#define METHOD_COMP_H

#include <string>
#include <list>
#include "../decafcomp.cc"

#include "llvm/symbol_table.h"
#include "llvm/type_utils.h"

/// ParameterAST
class ParameterAST : public decafAST {
    std::string Name;
    DecafType* Type;
public:
    ParameterAST(std::string name, DecafType* type) : Name(name), Type(type) {}
    ~ParameterAST() {}

    std::string getName() const { return Name; }
    DecafType* getType() const { return Type; }

    std::string str() override {
        return "VarDef(" + Name + "," + getString(Type) + ")";
    }

	llvm::Value* Codegen(CodegenContext& ctx) override {
        llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();
        llvm::Argument* llvmArg = nullptr;
        for (auto& arg : function->args()) {
            if (arg.getName() == Name) {
                llvmArg = &arg;
                break;
            }
        }

        if (!llvmArg)
            this->semantic_error("Parameter not found in function arguments: " + Name);

        llvm::IRBuilder<> TmpB(&function->getEntryBlock(), function->getEntryBlock().begin());
        llvm::Type* llvmType = getLLVMTypeFromString(Type->str(), ctx.llvmContext);

        llvm::AllocaInst* alloca = TmpB.CreateAlloca(llvmType, nullptr, Name);
        TmpB.CreateStore(llvmArg, alloca);

        Descriptor* desc = new Descriptor(Name, Descriptor::Kind::Parameter, alloca, llvmType);
        ctx.symbols.insert(Name, desc);

        return alloca;
    }
};

/// MethodType
class MethodType : public decafAST {
    std::string Type;
public:
    MethodType(const std::string& type) : Type(type) {}
    ~MethodType() {}

    std::string str() override {
        return Type;
    }

    std::string getTypeString() const {
        return Type;
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        return nullptr;
    }
};

/// MethodBlockAST
class MethodBlockAST : public decafAST {
    decafStmtList* FieldList;
    decafStmtList* StmtList;
public:
    MethodBlockAST(decafStmtList* fields, decafStmtList* stmts) : FieldList(fields), StmtList(stmts) {}
    ~MethodBlockAST() {
        delete FieldList;
        delete StmtList;
    }

    decafStmtList* getFields() const { return FieldList; }
    decafStmtList* getStmts() const { return StmtList; }

    std::string str() override {
        return "MethodBlock(" + getString(FieldList) + "," + getString(StmtList) + ")";
    }

	llvm::Value* Codegen(CodegenContext& ctx) override {
        llvm::Value* val = nullptr;
        if (FieldList) val = FieldList->Codegen(ctx);
        if (StmtList) val = StmtList->Codegen(ctx);
        if (!val)
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.llvmContext), 0);

        return val;
    }
};

/// MethodCallAST
class MethodCallAST : public decafAST {
    std::string Method;
    decafStmtList* Args;
public:
    MethodCallAST(std::string method, decafStmtList* args) : Method(method), Args(args) {}
    ~MethodCallAST() { delete Args; }

    std::string str() override {
        return "MethodCall(" + Method + "," + getString(Args) + ")";
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        // Check symbol table
        Descriptor* funcDesc = ctx.symbols.lookup(Method);
        if (!funcDesc)
            this->semantic_error("Function not in symbol table: " + Method);
        if (funcDesc->getKind() != Descriptor::Kind::Function)
            this->semantic_error("Identifier is not a function in this scope: " + Method);
        
        // Retrieve the function by name
        llvm::Function* CalleeF = llvm::dyn_cast<llvm::Function>(funcDesc->getValue());
        if (!CalleeF)
            this->semantic_error("Unknown function: " + Method);

		// Check argument count. Must match with function's count
		size_t expectedArgCount = CalleeF->getFunctionType()->getNumParams();
		size_t actualArgCount = Args ? Args->size() : 0;

		if (expectedArgCount != actualArgCount) {
			this->semantic_error("Function '" + Method
				+ "' expects " + std::to_string(expectedArgCount) + " argument(s)"
				+ ", but got " + std::to_string(actualArgCount));
		}

        // Generate arguments for the function call
        std::vector<llvm::Value*> ArgsV;
        unsigned idx = 0;
        for (auto stmt : *Args) {
            llvm::Value* arg = stmt->Codegen(ctx);
            if (!arg)
                this->semantic_error("Error generating argument for function: " + Method);

            llvm::Type* expectedType = CalleeF->getFunctionType()->getParamType(idx);
            llvm::Type* actualType = arg->getType();

            // If expected is i32 and actual is i1 (bool), cast
            if (actualType->isIntegerTy(1) && expectedType->isIntegerTy(32)) {
                arg = ctx.builder.CreateZExt(arg, ctx.builder.getInt32Ty(), "booltoint");
            }

            // Final strict type check
            if (arg->getType() != expectedType) {
                std::string actualTypeStr;
                std::string expectedTypeStr;

                llvm::raw_string_ostream actualStream(actualTypeStr);
                llvm::raw_string_ostream expectedStream(expectedTypeStr);

                arg->getType()->print(actualStream);
                expectedType->print(expectedStream);

                this->semantic_error(
                    "Argument type mismatch in call to function '" + Method +
                    "' at index " + std::to_string(idx) +
                    ":\n  Expected: " + expectedStream.str() +
                    "\n  Actual:   " + actualStream.str()
                );
            }

            ArgsV.push_back(arg);
            ++idx;
        }

        // Call function, return result or void
        if (CalleeF->getReturnType()->isVoidTy()) {
            ctx.builder.CreateCall(CalleeF, ArgsV);
            return nullptr;
        } else {
            return ctx.builder.CreateCall(CalleeF, ArgsV, Method + "_call");
        }
    }
};

class MethodAST : public decafAST {
    std::string MethodName;
    std::list<ParameterAST*> ParamList;
    MethodType* ReturnType;
    MethodBlockAST* Block;

public:
    MethodAST(const std::string& name,
              std::list<ParameterAST*> params,
              MethodType* type,
              MethodBlockAST* body)
        : MethodName(name), ParamList(params), ReturnType(type), Block(body) {}

    ~MethodAST() {
        for (auto param : ParamList) delete param;
        delete ReturnType;
        delete Block;
    }

    std::string str() override {
        std::string params = commaList<ParameterAST*>(ParamList);
        return "Method(" + MethodName + "," + getString(ReturnType) + "," +
               params + "," + getString(Block) + ")";
    }

    /// Pass 1: Declare the function and insert into the module
    llvm::Function* declareFunction(CodegenContext& ctx) {
		// Check for main specifically
		if (MethodName == "main") {
			// main exists already?
			if (ctx.hasMainFunction) {
				this->semantic_error("Multiple main functions found, only one main function allowed");
			}
			// main has parameters?
			if (!ParamList.empty()) {
				this->semantic_error("main function must have no parameters");
			}
			ctx.hasMainFunction = true;
		}


        llvm::Type* RetType = getLLVMTypeFromString(ReturnType->getTypeString(), ctx.llvmContext);

        if (ctx.symbols.is_declared_in_current_scope(MethodName))
            this->semantic_error("Identifier already exits in scope: " + MethodName);
        
        std::vector<llvm::Type*> ArgTypes;
        std::vector<std::string> ArgNames;

        for (auto param : ParamList) {
            ArgTypes.push_back(getLLVMTypeFromString(param->getType()->str(), ctx.llvmContext));
            ArgNames.push_back(param->getName());
        }

        std::string mangledName;
        if (MethodName == "main"){
            mangledName = MethodName;
        }else{
            mangledName = ctx.module->getModuleIdentifier() + "_" + MethodName;
        }
        llvm::FunctionType* FT = llvm::FunctionType::get(RetType, ArgTypes, false);
        llvm::Function* F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, mangledName, ctx.module);

        Descriptor* funcDesc = new Descriptor(MethodName, F, ArgTypes);
        ctx.symbols.insert(MethodName, funcDesc);

        unsigned idx = 0;
        for (auto& arg : F->args()) {
            arg.setName(ArgNames[idx++]);
        }

        return F;
    }

    /// Pass 2: Codegen the method body
    llvm::Value* Codegen(CodegenContext& ctx) override {

        // Check symbol table
        Descriptor* funcDesc = ctx.symbols.lookup(MethodName);
        if (!funcDesc)
            this->semantic_error("Function not in symbol table: " + MethodName);
        if (funcDesc->getKind() != Descriptor::Kind::Function)
            this->semantic_error("Identifier is not a function in this scope: " + MethodName);
        
        // Retrieve the function by name
        llvm::Function* F = llvm::dyn_cast<llvm::Function>(funcDesc->getValue());
        if (!F) 
            this->semantic_error("Function not declared: " + MethodName);

        llvm::BasicBlock* BB = llvm::BasicBlock::Create(ctx.llvmContext, "entry", F);
        ctx.builder.SetInsertPoint(BB);
        ctx.symbols.push_scope();

        // Allocate and store parameters
        for (auto& arg : F->args()) {
            llvm::IRBuilder<> TmpB(&F->getEntryBlock(), F->getEntryBlock().begin());
            llvm::AllocaInst* alloca = TmpB.CreateAlloca(arg.getType(), nullptr, arg.getName());
            TmpB.CreateStore(&arg, alloca);

            Descriptor* paramDesc = new Descriptor(arg.getName().str(), Descriptor::Kind::Parameter, alloca, arg.getType());
            ctx.symbols.insert(arg.getName().str(), paramDesc);
        }

        llvm::Value* bodyVal = Block->Codegen(ctx);
        if (!bodyVal) {
            F->eraseFromParent();
            this->semantic_error("Error generating body for method: " + MethodName);
        }

        if (!ctx.builder.GetInsertBlock()->getTerminator()) {
            if (F->getReturnType()->isVoidTy()) {
                if (MethodName == "main") {
                    this->semantic_error("Main should only return int, not void");
                } else {
                    ctx.builder.CreateRetVoid();
                }
            }
            else if (F->getReturnType()->isIntegerTy(1)){
                ctx.builder.CreateRet(llvm::ConstantInt::get(F->getReturnType(), 1));
            }
            else {
                ctx.builder.CreateRet(llvm::ConstantInt::get(F->getReturnType(), 0));
            }
        }

        ctx.symbols.pop_scope();
        return F;
    }
};

class PackageAST : public decafAST {
	string Name;
	decafStmtList *FieldDeclList;
	decafStmtList *MethodDeclList;
public:
	PackageAST(string name, decafStmtList *fieldlist, decafStmtList *methodlist) 
		: Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}
	
	~PackageAST() { 
		if (FieldDeclList != NULL) { delete FieldDeclList; }
		if (MethodDeclList != NULL) { delete MethodDeclList; }
	}

	string str() override { 
		return string("Package") + "(" + Name + "," + getString(FieldDeclList) + "," + getString(MethodDeclList) + ")";
	}

	llvm::Value *Codegen(CodegenContext& ctx) override {
		ctx.module->setModuleIdentifier(Name);
		llvm::Value *val = nullptr;

		if (FieldDeclList) val = FieldDeclList->Codegen(ctx);

		// First pass: declare all methods
		for (auto stmt : *MethodDeclList) {
			MethodAST* method = dynamic_cast<MethodAST*>(stmt);
			if (method) {
				method->declareFunction(ctx);
			}
		}

		// Second pass: codegen all method bodies
		for (auto stmt : *MethodDeclList) {
			MethodAST* method = dynamic_cast<MethodAST*>(stmt);
			if (method) {
				method->Codegen(ctx);
			}
		}

		// Check if main is found
		if (!ctx.hasMainFunction) {
			this->semantic_error("No main function found in program");
		}

		return val;
	}
};

/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *PackageDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {}
	~ProgramAST() { 
		if (ExternList != NULL) { delete ExternList; } 
		if (PackageDef != NULL) { delete PackageDef; }
	}
	string str() { return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }
	llvm::Value *Codegen(CodegenContext& ctx) { 
		llvm::Value *val = nullptr;
		if (ExternList) {
			val = ExternList->Codegen(ctx);
		}
        ctx.symbols.push_scope();
		if (PackageDef) {
			val = PackageDef->Codegen(ctx);
		} else {
            this->semantic_error("No package definition in decaf program");
		}
        ctx.symbols.pop_scope();
		return val;
	}
};

#endif // METHOD_COMP_H