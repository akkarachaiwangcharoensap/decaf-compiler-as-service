#ifndef EXTERN_COMP_H
#define EXTERN_COMP_H

#include <string>
#include "../decafcomp.cc"  // Include the base definitions (decafAST, decafStmtList, getString)
#include "method_comp.h"

class ExternType : public decafAST {
    string Type;
public:
    ExternType(const string& type) : Type(type) {}
    ~ExternType() {}

    string str() override {
        return Type;
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
		// For now, if it doesn't generate code, return nullptr
        throw std::runtime_error("ExternType should not generate code");
        return nullptr;
	}
};

/// ExternAST - Extern declaration
class ExternAST : public decafAST {
    string ExternName;
    decafStmtList* ParamTypes;
    MethodType* ReturnType;
public:
    ExternAST(string name, decafStmtList* params, MethodType* type)
        : ExternName(name), ParamTypes(params), ReturnType(type) {}
    ~ExternAST() { delete ParamTypes; }
    string str() override {
        return "ExternFunction(" + ExternName + "," + getString(ReturnType) + "," + getString(ParamTypes) + ")";
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        std::vector<llvm::Type*> paramTypes;

        
        if(ctx.symbols.Does_Identifier_Already_Exist_In_Scope(ExternName)){
            throw std::runtime_error("Identifier already exits in scope " + ExternName);
        }
        // Convert ParamTypes (decafStmtList*) to llvm::Type*
        for (auto param : *ParamTypes) {
            ExternType* externType = dynamic_cast<ExternType*>(param);
            if (!externType) {
                throw std::runtime_error("Invalid extern parameter type");
            }

            std::string t = externType->str();
            if (t.find("IntType") != std::string::npos) {
                paramTypes.push_back(llvm::Type::getInt32Ty(ctx.llvmContext));
            } else if (t.find("BoolType") != std::string::npos) {
                paramTypes.push_back(llvm::Type::getInt1Ty(ctx.llvmContext));
            } else if (t.find("StringType") != std::string::npos) {
                paramTypes.push_back(llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(ctx.llvmContext)));
            }
            else {
                throw std::runtime_error("Unsupported extern parameter type: " + t);
            }
        }

        // Return type
        llvm::Type* retType;
        std::string retStr = ReturnType->str();
        if (retStr == "VoidType") {
            retType = llvm::Type::getVoidTy(ctx.llvmContext);
        } else if (retStr == "IntType") {
            retType = llvm::Type::getInt32Ty(ctx.llvmContext);
        } else if (retStr == "BoolType") {
            retType = llvm::Type::getInt1Ty(ctx.llvmContext);
        } else {
            throw std::runtime_error("Unsupported extern return type: " + retStr);
        }

        llvm::FunctionType* funcType = llvm::FunctionType::get(retType, paramTypes, false);
        llvm::Function* function = llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            ExternName,
            ctx.module
        );

        Descriptor* funcDesc = new Descriptor(ExternName, function, paramTypes);
        ctx.symbols.insert(ExternName, funcDesc);


        char idx = 'A';
        for (auto& arg : function->args()) {
            arg.setName(std::string(1, idx++));
        }

        return function;
    }
};

#endif // EXTERN_COMP_H