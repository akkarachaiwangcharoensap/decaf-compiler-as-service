#ifndef FIELD_COMP_H
#define FIELD_COMP_H

#include <string>
#include <cstdint>
#include <cctype>

#include "llvm/type_utils.h"
#include "../decafcomp.cc"

#define MAX_ARRAY_SIZE 1000000

class ArrayType : public decafAST {
    DecafType* Type;
    string Size;
public:
    ArrayType(DecafType* type, const string& size)
        : Type(type), Size(size)  {}

    ~ArrayType() {}

    string str() override {
        return getString(Type) + ",Array(" + Size + ")";
    }

    DecafType* getType() {
        return Type;
    }

    string getSize() {
        return Size;
    }
    
    llvm::Value *Codegen(CodegenContext& ctx) override {
		// For now, if it doesn't generate code, return nullptr
        this->semantic_error("ArrayType should not generate code");
        return nullptr;
	}
};

class ScalarType : public decafAST {
public:
    ScalarType() {}
    ~ScalarType() {}
    string str() override {
        return "Scalar";
    }
    llvm::Value *Codegen(CodegenContext& ctx) override {
		// For now, if it doesn't generate code, return nullptr
        this->semantic_error("ScalarType should not generate code");
        return nullptr;
	}
};

/// ConstantAST - Right hand-side value
class ConstantAST : public decafAST {
    string Type;
    string Value;
public:
    ConstantAST(string type, string value) : Type(type), Value(value) {}
    string str() override {
        return Type + "(" + Value + ")";
    }

    int32_t parseAndWrapToI32(const string &input) {
        uint64_t result = 0;
        uint64_t modBase = (1ULL << 32);
        int base = 10;  // default decimal
        size_t start = 0;

        // Hex detection
        if (input.size() > 2 && input[0] == '0' && (input[1] == 'x' || input[1] == 'X')) {
            base = 16;
            start = 2;
        }

        // Parse each character
        for (size_t i = start; i < input.size(); i++) {
            char c = input[i];
            int digit = -1;

            if (isdigit(c)) digit = c - '0';
            else if (base == 16 && isxdigit(c)) {
                digit = 10 + (tolower(c) - 'a');
            }

            if (digit >= 0 && digit < base) {
                result = (result * base + digit) % modBase;
            }
        }

        return static_cast<int32_t>(result);
    }

    std::string parseStringLiteral(const std::string& s) {
        std::string unquoted = s;
        if (s.length() >= 2 && s.front() == '"' && s.back() == '"') {
            unquoted = s.substr(1, s.length() - 2);
        }

        std::string result;
        for (size_t i = 0; i < unquoted.length(); ++i) {
            if (unquoted[i] == '\\' && i + 1 < unquoted.length()) {
                char next = unquoted[++i];
                switch (next) {
                    case 'n': result += '\n'; break;
                    case 't': result += '\t'; break;
                    case 'v': result += '\v'; break;
                    case 'r': result += '\r'; break;
                    case 'a': result += '\a'; break;
                    case 'f': result += '\f'; break;
                    case 'b': result += '\b'; break;
                    case '\\': result += '\\'; break;
                    case '"': result += '\"'; break;
                    default: result += next; break;
                }
            } else {
                result += unquoted[i];
            }
        }
        return result;
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        if (Type == "NumberExpr") {
            int32_t intValue = parseAndWrapToI32(Value);
            return llvm::ConstantInt::get(ctx.builder.getInt32Ty(), intValue, true);

        } else if (Type == "BoolExpr") {
            bool boolValue = (Value == "True");
            return llvm::ConstantInt::get(ctx.builder.getInt1Ty(), boolValue, false);
        }
        else if (Type == "StringConstant") {
            std::string decoded = parseStringLiteral(Value);
            return ctx.builder.CreateGlobalString(decoded);
        }
        else {
            this->semantic_error("Unsupported constant type: " + Type);
        }

        return nullptr;
    }
};

/// SimpleIDAST - identifier wrapper.
class SimpleIDAST : public decafAST {
    std::string ID;

public:
    SimpleIDAST(std::string id) : ID(id) {}

    std::string str() override {
        return "" + ID + "";
    }
    llvm::Value *Codegen(CodegenContext& ctx) override {
		// For now, if it doesn't generate code, return nullptr
        this->semantic_error("SimpleIDAST should not generate code");
        return nullptr;
	}
};

/// FieldDeclAST - the decaf global variable declaration.
class FieldDeclAST : public decafAST {
    DecafType* Type;
    decafStmtList* IdentList;
    decafAST* Initializer;

public:
    FieldDeclAST(DecafType* type, decafStmtList* ids, decafAST* init = nullptr)
        : Type(type), IdentList(ids), Initializer(init) {}

    ~FieldDeclAST() {
        delete IdentList;
        if (Initializer) delete Initializer;
    }

    string str() override {
        string result;
        for (auto stmt : *IdentList) {
            result += "FieldDecl(" + stmt->str() + "," + getString(Type);
            if (Initializer) {
                result += "," + getString(Initializer);
            }

            result += "),";
        }

        if (!result.empty()) {
            result.pop_back();
        }

        return result;
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        llvm::Type* llvmType = getLLVMTypeFromString(getString(Type), ctx.llvmContext);

        for (auto stmt : *IdentList) {
            SimpleIDAST* id = dynamic_cast<SimpleIDAST*>(stmt);
            if (!id)
                this->semantic_error("Invalid identifier in global variable declaration");

            std::string varName = id->str();

            if(ctx.symbols.is_declared_in_current_scope(varName))
                this->semantic_error("Identifier already exists in scope '" + varName + "'");

            // Create the global variable with external linkage
            llvm::GlobalVariable* gvar = new llvm::GlobalVariable(
                *ctx.module,
                llvmType,
                false, // isConstant
                llvm::GlobalValue::ExternalLinkage,

                // Get null value returns 0 if null (see zero-initializers testcase)
                llvm::Constant::getNullValue(llvmType),
                varName
            );

            ctx.symbols.insert(varName, new Descriptor(
                varName, Descriptor::Kind::Global, gvar, llvmType
            ));
        }

        return nullptr;
    }
};

/// ArrayDeclAST - the decaf global variable declaration.
class ArrayDeclAST : public decafAST {
    decafStmtList* IdentList;
    decafAST* ArrayAST;
    
public:
    ArrayDeclAST(decafStmtList* ids, decafAST* arrayAST)
        : ArrayAST(arrayAST), IdentList(ids) {}

    ~ArrayDeclAST() {
        delete IdentList;
    }

    string str() override {
        string result;
        for (auto stmt : *IdentList) {
            result += "FieldDecl(" + stmt->str() + "," + getString(ArrayAST) + "),";
        }

        if (!result.empty())
            result.pop_back();

        return result;
    }

    int32_t parseAndWrapToI32(const string &input) {
        uint64_t result = 0;
        uint64_t modBase = (1ULL << 32);
        int base = 10;  // default decimal
        size_t start = 0;

        // Hex detection
        if (input.size() > 2 && input[0] == '0' && (input[1] == 'x' || input[1] == 'X')) {
            base = 16;
            start = 2;
        }

        // Parse each character
        for (size_t i = start; i < input.size(); i++) {
            char c = input[i];
            int digit = -1;

            if (isdigit(c)) digit = c - '0';
            else if (base == 16 && isxdigit(c)) {
                digit = 10 + (tolower(c) - 'a');
            }

            if (digit >= 0 && digit < base) {
                result = (result * base + digit) % modBase;
            }
        }

        return static_cast<int32_t>(result);
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        for (auto stmt : *IdentList) {
            SimpleIDAST* id = dynamic_cast<SimpleIDAST*>(stmt);
            if (!id)
                this->semantic_error("Invalid identifier in array declaration");

            std::string arrayName = id->str();
            if(ctx.symbols.is_declared_in_current_scope(arrayName))
                this->semantic_error("Identifier already exists in scope '" + arrayName + "'");
            
            ArrayType* arrType = dynamic_cast<ArrayType*>(ArrayAST);
            if (!arrType)
                this->semantic_error("Expected ArrayType in ArrayDeclAST");

            DecafType* elemType = arrType->getType();
            std::string elemTypeStr = getString(elemType);
            llvm::Type* llvmElemType = getLLVMTypeFromString(elemTypeStr, ctx.llvmContext);

            int arraySize = parseAndWrapToI32(arrType->getSize());
            if (arraySize <= 0)
                this->semantic_error("Array index must be greater than zero: " + std::to_string(arraySize));
            
            if (arraySize > MAX_ARRAY_SIZE) {
                this->semantic_error(
                    "Array size exceeds compiler maximum (" +
                    std::to_string(MAX_ARRAY_SIZE) + "): " +
                    std::to_string(arraySize)
                );
            }


            llvm::ArrayType* llvmArrayTy = llvm::ArrayType::get(llvmElemType, arraySize);
            llvm::Constant* zeroInit = llvm::ConstantAggregateZero::get(llvmArrayTy);

            llvm::GlobalVariable* gvar = new llvm::GlobalVariable(
                *ctx.module,
                llvmArrayTy,
                false,
                llvm::GlobalValue::ExternalLinkage,
                zeroInit,
                arrayName
            );

            ctx.symbols.insert(arrayName, new Descriptor(
                arrayName, Descriptor::Kind::Global, gvar, llvmArrayTy
            ));
        }

        return nullptr;
    }
};

class FieldAssignAST : public decafAST {
    DecafType* Type;
    decafStmtList* IdentList;
    ConstantAST* Constant;

public:
    FieldAssignAST(DecafType* type, decafStmtList* ids, ConstantAST* constant)
        : Type(type), IdentList(ids), Constant(constant) {}

    ~FieldAssignAST() {
        delete IdentList;
    }

    std::string str() override {
        return "AssignGlobalVar(" 
            + getString(IdentList) + 
            "," + getString(Type) + 
            "," + getString(Constant)
        + ")";
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        // Generate the constant value to assign
        llvm::Value* value = Constant->Codegen(ctx);
        if (!value)
            this->semantic_error("Failed to generate constant value in FieldAssignAST");

        llvm::Type* llvmType = getLLVMTypeFromString(getString(Type), ctx.llvmContext);

        // Cast the constant to llvm::Constant for global initialization
        llvm::Constant* initVal = llvm::dyn_cast<llvm::Constant>(value);
        if (!initVal)
            this->semantic_error("Global initializer must be a constant");

        for (auto stmt : *IdentList) {
            SimpleIDAST* id = dynamic_cast<SimpleIDAST*>(stmt);
            if (!id)
                this->semantic_error("Invalid identifier in global assignment");

            std::string varName = id->str();

            if(ctx.symbols.is_declared_in_current_scope(varName))
                this->semantic_error("Identifier already exists in scope '" + varName + "'");
            
            // Create a global variable with the constant initializer
            llvm::GlobalVariable* gvar = new llvm::GlobalVariable(
                *ctx.module,
                llvmType,
                false, // not constant
                llvm::GlobalValue::ExternalLinkage,
                initVal,
                varName
            );

            // Insert into symbol table
            ctx.symbols.insert(varName, new Descriptor(
                varName, Descriptor::Kind::Global, gvar, llvmType
            ));
        }

        return nullptr;
    }
};

/// VarDeclAST - the decaf variable declaration.
class VarDeclAST : public decafAST {
    DecafType* Type;
    decafStmtList* IdentList;

public:
    VarDeclAST(DecafType* type, decafStmtList* ids)
        : Type(type), IdentList(ids) {}

    ~VarDeclAST() {
        delete IdentList;
    }

    string str() override {
        string result;
        for (auto stmt : *IdentList) {
            result += "VarDef(" + stmt->str() + "," + getString(Type) + "),";
        }

        if (!result.empty())
            result.pop_back();

        return result;
    }

    llvm::Value *Codegen(CodegenContext& ctx) override {
        for (auto stmt : *IdentList) {
            SimpleIDAST* id = dynamic_cast<SimpleIDAST*>(stmt);
            if (!id)
                this->semantic_error("Invalid identifier in variable declaration");

            std::string varName = id->str();

            if(ctx.symbols.is_declared_in_current_scope(varName))
                this->semantic_error("Identifier already exists in scope '" + varName + "'");
            
            llvm::Type* llvmType = getLLVMTypeFromString(getString(Type), ctx.llvmContext);
            llvm::Value* alloca = ctx.builder.CreateAlloca(llvmType, nullptr, varName);

            ctx.builder.CreateStore(llvm::Constant::getNullValue(llvmType), alloca);
            ctx.symbols.insert(varName, new Descriptor(
                varName, Descriptor::Kind::Variable, alloca, llvmType
            ));
        }

        return nullptr;
    }
};

class ArrayIDAST : public decafAST {
    std::string ID;
    decafAST* Index;

public:
    ArrayIDAST(std::string id, decafAST* index) : ID(id), Index(index) {}

    std::string getID() { return ID; }
    decafAST* getIndex() { return Index; }

    std::string str() override {
        return "ArrayLocExpr(" + ID + "," + Index->str() + ")";
    }

    llvm::Value* getArrayElementPtr(CodegenContext& ctx) {
        Descriptor* desc = ctx.symbols.lookup(ID);
        if (!desc)
            this->semantic_error("Undeclared array '" + ID + "'");

        llvm::Value* arrayPtr = desc->getValue();
        llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(desc->getType());
        if (!arrayType)
            this->semantic_error("Expected array type for: " + ID);

        llvm::Value* zero = llvm::ConstantInt::get(ctx.builder.getInt32Ty(), 0);
        // Evaluate the index AST expression
        llvm::Value* indexVal = Index->Codegen(ctx);
        if (!indexVal)
            this->semantic_error("Invalid array index expression");

        if (!indexVal->getType()->isIntegerTy(32))
            this->semantic_error("Array index must be an integer: " + Index->str());

        return ctx.builder.CreateGEP(
            arrayType,
            arrayPtr,
            {zero, indexVal},
            ID + "_elt"
        );
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        llvm::Value* elemPtr = getArrayElementPtr(ctx);

        Descriptor* desc = ctx.symbols.lookup(ID);
        if (!desc) 
            this->semantic_error("Undeclared array: " + ID);

        llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(desc->getType());
        if (!arrayType) 
            this->semantic_error("Expected array type for: " + ID);

        llvm::Type* elemType = arrayType->getElementType();
        return ctx.builder.CreateLoad(elemType, elemPtr, ID + "_val");
    }
};

#endif // FIELD_COMP_H