#ifndef CODEGEN_CONTEXT_H
#define CODEGEN_CONTEXT_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "symbol_table.h"
#include <stack>

struct CodegenContext {
    // Global LLVM context used to manage types and other LLVM-level info
    llvm::LLVMContext& llvmContext;

    // Instruction builder used to generate LLVM IR
    llvm::IRBuilder<>& builder;

    // The LLVM module being built (e.g., one .ll file or translation unit)
    llvm::Module* module;

    // Stack of symbol tables for tracking variable scopes
    SymbolTableStack symbols;

    // Stack of blocks to jump to on 'break' (used in loops)
    std::stack<llvm::BasicBlock*> loopBreakStack;

    // Stack of blocks to jump to on 'continue' (used in loops)
    std::stack<llvm::BasicBlock*> loopContinueStack;
    
    CodegenContext(llvm::LLVMContext& ctx, llvm::IRBuilder<>& b, llvm::Module* m)
        : llvmContext(ctx), builder(b), module(m) {}
};

#endif // CODEGEN_CONTEXT_H