#ifndef TYPE_UTILS_H
#define TYPE_UTILS_H

#include <string>
#include <stdexcept>
#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"

// Map string type into LLVM IR code.
inline llvm::Type* getLLVMTypeFromString(const std::string& typeStr, llvm::LLVMContext& ctx) {
    if (typeStr == "IntType") {
        return llvm::Type::getInt32Ty(ctx);
    } else if (typeStr == "BoolType") {
        return llvm::Type::getInt1Ty(ctx);
    } else if (typeStr == "VoidType") {
        return llvm::Type::getVoidTy(ctx);
    } else if (typeStr == "StringType") {
        return llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(ctx));
    } else {
        throw std::runtime_error("Unknown type string: " + typeStr);
    }
}

#endif // TYPE_UTILS_H