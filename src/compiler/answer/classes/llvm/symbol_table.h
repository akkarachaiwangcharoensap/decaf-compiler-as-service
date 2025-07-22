// g++ -std=c++17 -o symtbl_test symtbl_test.c
#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include <utility>      // std::pair, std::make_pair
#include <string>       // std::string
#include <iostream>     // std::cout
#include <map>          // std::map
#include <list>         // std::list

using namespace std;

class Descriptor {
public:
    enum class Kind { Variable, Function, Parameter, Global };


private:
    std::string name;
    Kind kind;
    llvm::Value* llvmValue;              // For variables: AllocaInst; For functions: llvm::Function*
    llvm::Type* llvmType;                // The LLVM type of the value
    std::vector<llvm::Type*> paramTypes; // For functions
    int paramCount = 0;

public:
    // Constructor for variable/parameter
    Descriptor(std::string name, Kind kind, llvm::Value* value, llvm::Type* type)
        : name(std::move(name)), kind(kind), llvmValue(value), llvmType(type) {}

    // Constructor for function
    Descriptor(std::string name, llvm::Function* function, std::vector<llvm::Type*> paramTypes)
        : name(std::move(name)), kind(Kind::Function), llvmValue(function),
          llvmType(function->getFunctionType()), paramTypes(std::move(paramTypes)), paramCount(paramTypes.size()) {}

    Kind getKind() const { return kind; }
    llvm::Value* getValue() const { return llvmValue; }
    llvm::Type* getType() const { return llvmType; }
    const std::string& getName() const { return name; }

    int getParamCount() const { return paramCount; }
    const std::vector<llvm::Type*>& getParamTypes() const { return paramTypes; }

    void print() const {
        std::cout << "Descriptor(" << name << ", ";
        switch (kind) {
            case Kind::Variable: std::cout << "Variable"; break;
            case Kind::Function: std::cout << "Function"; break;
            case Kind::Parameter: std::cout << "Parameter"; break;
            case Kind::Global:   std::cout << "Global"; break;
        }
        std::cout << ", llvm::Value*=" << llvmValue;
        if (kind == Kind::Function) {
            std::cout << ", params: " << paramCount << ")";
        } else {
            std::cout << ")";
        }
        std::cout << "\n";
    }
};

class SymbolTableStack {
    using symbol_table = map<string, Descriptor*>;
    list<symbol_table> stack;

public:
    void push_scope() {
        stack.emplace_front(); // Add a new scope
    }

    void pop_scope() {
        if (!stack.empty()) stack.pop_front(); // Remove the current scope
    }

    void insert(string ident, Descriptor* desc) {
        if (!stack.empty()) {
            stack.front()[ident] = desc;
        }
    }

    Descriptor* lookup(const string& ident) {
        for (auto& scope : stack) {
            auto it = scope.find(ident);
            if (it != scope.end()) {
                return it->second;
            }
        }
        return nullptr;
    }

    void print() {
        for (const auto& scope : stack) {
            for (const auto& entry : scope) {
                cout << entry.first << " -> ";
                entry.second->print();
                cout << endl;
            }
        }
    }
};

#endif  // SYMBOLTABLE_H